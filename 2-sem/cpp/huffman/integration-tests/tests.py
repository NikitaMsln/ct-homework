import random
import sys
import tempfile
import unittest
import os
import subprocess
import hashlib

import requests

TESTS_DIR = os.path.join(os.getcwd(), 'integration-tests', 'data')
CHUNK_SIZE = 1024 * 8
CI_RUN = 'CI_RUN' in os.environ
DEBUG = 'NO_DEBUG' not in os.environ

random.seed(1337)


def debug(*args, end='\n'):
    if DEBUG:
        print(*args, end=end)


def find_tool():
    name = 'huffman-tool' if sys.platform.lower() != 'windows' else 'huffman-tool.exe'
    for root, dirs, files in os.walk(os.getcwd()):
        if name in files:
            return os.path.join(root, name)


def file_checksum(filename):
    checksum = hashlib.md5()
    with open(filename, 'rb') as file:
        for chunk in iter(lambda: file.read(CHUNK_SIZE), b''):
            checksum.update(chunk)
    return checksum.hexdigest()


def create_command(args, profiling=False, shuffle=False):
    # TODO windows?
    command = ['time', '-f', '%e'] if profiling else []

    if shuffle:
        random.shuffle(args)
    command += [find_tool()] + [item for pair in args for item in pair]
    command = list(filter(lambda x: x != '', command))
    # debug(command)
    return command


def run_command(command):
    with subprocess.Popen(command, stderr=subprocess.PIPE) as sub:
        output = sub.stderr.read()
        sub.wait()
        return_code = sub.returncode
    return output, return_code


class TestCaseBase(unittest.TestCase):
    orig = None
    comp = None
    decomp = None

    @classmethod
    def setUpClass(cls):
        cls.comp = cls.orig + '.huf'
        orig, ext = os.path.splitext(cls.orig)
        cls.decomp = orig + '_decompressed' + ext

    def download_orig(self, url):
        if os.path.exists(self.orig):
            return
        debug(f'\nDownloading file {self.orig} for test, this may take time, please wait...')
        r = requests.get(url, stream=True)
        self.assertEqual(r.status_code, 200, 'Response code for file download not 200')
        with open(self.orig, 'wb') as f:
            for chunk in r:
                f.write(chunk)

    def run_tool_custom(self, mode, command, fo, expect_error=False, profiling=False, limit=100.):
        output, return_code = run_command(command)
        # debug(return_code)
        split_output = output.split(b'\n')
        # debug(output, len(split_output))
        if expect_error:
            self.assertNotEqual(return_code, 0, 'Program is expected to indicate an error by returning non '
                                                'zero exit code')
            self.assertGreaterEqual(len(split_output), 2 + int(profiling), 'Program did not print anything to stderr '
                                                                           'while returned error')
        else:
            self.assertEqual(return_code, 0, 'Program had not finished successfully (return code not zero)')

        if profiling:
            elapsed = float(split_output[-2])
            self.assertLessEqual(elapsed, limit, f'Command in {mode} mode took too much time')

        if not expect_error:
            self.assertTrue(os.path.exists(fo), f'Output file in {mode} mode was not created')

    def run_tool_common(self, mode, expect_error=False, profiling=False, limit=100., more_args=None):
        if more_args is None:
            more_args = []
        fi, fo = (self.orig, self.comp) if (mode == 'compress') else (self.comp, self.decomp)

        args = [
            (f'--{mode}', ''),
            ('--input', fi),
            ('--output', fo)
        ]
        command = create_command(args, profiling=profiling) + more_args

        self.run_tool_custom(mode, command, fo, expect_error, profiling, limit)

    def run_correctness(self):
        self.run_tool_common('compress')
        self.run_tool_common('decompress')
        with open(self.orig, 'rb') as original:
            with open(self.decomp, 'rb') as decompressed:
                self.assertEqual(original.read(), decompressed.read(), 'Original and decompressed files do not match')

    def run_speed(self, comp_limit, decomp_limit):
        self.run_tool_common('compress', profiling=True, limit=comp_limit)
        self.run_tool_common('decompress', profiling=True, limit=decomp_limit)

    def run_compression_ratio(self, expected_ratio):
        self.run_tool_common('compress')
        self.run_tool_common('decompress')
        original_size = os.path.getsize(self.orig)
        compressed_size = os.path.getsize(self.comp)
        decompressed_size = os.path.getsize(self.decomp)
        self.assertEqual(original_size, decompressed_size)
        self.assertGreaterEqual(original_size / compressed_size, expected_ratio)

    @classmethod
    def tearDownClass(cls):
        if (cls.comp is not None) and os.path.exists(cls.comp):
            os.remove(cls.comp)
        if (cls.decomp is not None) and os.path.exists(cls.decomp):
            os.remove(cls.decomp)


class TestSimpleFile(TestCaseBase):
    @classmethod
    def setUpClass(cls):
        cls.orig = os.path.join(TESTS_DIR, 'simple.txt')
        super().setUpClass()

    def test_correctness(self):
        self.run_correctness()

    def test_speed(self):
        self.run_speed(0.01, 0.01)

    def test_compression_ratio(self):
        self.run_compression_ratio(1.6)

    def test_wrong_args(self):
        self.run_tool_common('compress', more_args=['1337'], expect_error=True)
        self.run_tool_common('compress', more_args=['--decompress'], expect_error=True)

    def test_shuffled_args(self):
        command1 = create_command([('--output', self.comp), ('--input', self.orig), ('--compress', '')])
        self.run_tool_custom('compress', command1, self.comp)
        command2 = create_command([('--input', self.comp), ('--decompress', ''), ('--output', self.decomp)])
        self.run_tool_custom('decompress', command2, self.decomp)
        with open(self.orig, 'rb') as original:
            with open(self.decomp, 'rb') as decompressed:
                self.assertEqual(original.read(), decompressed.read(), 'Original and decompressed files do not match')


class TestRealFile(TestCaseBase):
    @classmethod
    def setUpClass(cls):
        cls.orig = os.path.join(TESTS_DIR, 'war_and_peace.txt')
        super().setUpClass()

    def test_correctness(self):
        self.run_correctness()

    def test_speed(self):
        self.run_speed(0.54, 0.7)


class TestMissingFile(TestCaseBase):
    @classmethod
    def setUpClass(cls):
        cls.orig = os.path.join(TESTS_DIR, 'i_do_not_exist.txt')
        super().setUpClass()

    def test_error(self):
        self.run_tool_common('compress', expect_error=True)


class TestRestrictedFile(TestCaseBase):
    @classmethod
    def setUpClass(cls):
        cls.orig = os.path.join(TESTS_DIR, 'restricted.txt')
        os.chmod(cls.orig, 0o000)
        super().setUpClass()

    def test_error(self):
        if CI_RUN:
            self.skipTest('Can not change permissions on CI because it runs in root')
        self.run_tool_common('compress', expect_error=True)

    @classmethod
    def tearDownClass(cls):
        os.chmod(cls.orig, 0o644)


class TestNotArchiveDecompress(TestCaseBase):
    @classmethod
    def setUpClass(cls):
        cls.orig = os.path.join(TESTS_DIR, 'simple.txt')
        super().setUpClass()
        cls.comp = cls.orig

    def test_error(self):
        self.run_tool_common('decompress', expect_error=True)

    @classmethod
    def tearDownClass(cls):
        cls.comp = None
        super().tearDownClass()


class TestEmptyFile(TestCaseBase):
    @classmethod
    def setUpClass(cls):
        cls.orig = os.path.join(TESTS_DIR, 'empty.txt')
        super().setUpClass()

    def test_correctness(self):
        self.run_correctness()

    def test_compression_ratio(self):
        # compressed empty file must hold the tree, so it must not be empty
        self.run_tool_common('compress')
        self.run_tool_common('decompress')
        compressed_size = os.path.getsize(self.comp)
        decompressed_size = os.path.getsize(self.decomp)

        self.assertEqual(decompressed_size, 0, 'Decompressed file must be empty')
        self.assertGreater(compressed_size, 0, 'Compressed file must not be empty')


class TestRandomBytesFile(TestCaseBase):
    @classmethod
    def setUpClass(cls):
        cls.orig = os.path.join(TESTS_DIR, 'random_bytes')
        super().setUpClass()

    def test_correctness(self):
        self.run_correctness()

    def test_speed(self):
        self.run_speed(0.01, 0.01)

    def test_compression_ratio(self):
        # distribution is uniform + dictionary saving
        self.run_compression_ratio(0.87)


class TestSomePDF(TestCaseBase):
    @classmethod
    def setUpClass(cls):
        cls.orig = os.path.join(TESTS_DIR, 'basov.pdf')
        super().setUpClass()

    def setUp(self):
        self.download_orig('https://hazzus.github.io/basov.pdf')

    def test_big(self):
        self.run_tool_common('compress', profiling=True, limit=4.)
        self.run_tool_common('decompress', profiling=True, limit=6.)

        original_size = os.path.getsize(self.orig)
        compressed_size = os.path.getsize(self.comp)
        decompressed_size = os.path.getsize(self.decomp)
        self.assertEqual(original_size, decompressed_size)
        self.assertEqual(file_checksum(self.orig), file_checksum(self.decomp))

        # pdf is binary and compresses not so good as well
        self.assertGreaterEqual(original_size / compressed_size, 1.2)


class TestSome8KJPG(TestCaseBase):
    @classmethod
    def setUpClass(cls):
        cls.orig = os.path.join(TESTS_DIR, 'buggati.jpg')
        super().setUpClass()

    def setUp(self):
        self.download_orig('https://img1.akspic.ru/attachments/crops/2/9/6/7/6/167692/167692-b'
                           'ugatti-bugatti_shiron_sport-bugatti_chiron_chistyj_sport-legkovyye'
                           '_avtomobili-sportkar-7680x4320.jpg')

    def test_big(self):
        self.run_tool_common('compress', profiling=True, limit=1.2)
        self.run_tool_common('decompress', profiling=True, limit=0.8)

        original_size = os.path.getsize(self.orig)
        compressed_size = os.path.getsize(self.comp)
        decompressed_size = os.path.getsize(self.decomp)
        self.assertEqual(original_size, decompressed_size)
        self.assertEqual(file_checksum(self.orig), file_checksum(self.decomp))

        # jpg is binary and compresses not so good as well
        self.assertGreaterEqual(original_size / compressed_size, 1.0001)


class TestRandomDirectories(unittest.TestCase):

    def batch_compare(self, first, second):
        with open(first, 'rb') as f:
            with open(second, 'rb') as s:
                self.assertEqual(f.read(CHUNK_SIZE), s.read(CHUNK_SIZE), f'Files do not match: {first} and {second}')

    def run_dir(self, dir, random_help=0.):
        tmp = tempfile.gettempdir()
        max_len = 0
        for (dirr, _, filenames) in os.walk(dir):
            for filename in filenames:
                path = os.path.join(dirr, filename)
                if not os.access(path, os.R_OK):
                    continue
                if dirr.endswith(os.path.join('integration-tests', 'data')):
                    continue
                if random.random() < random_help:
                    continue
                max_len = max(max_len, len(path))
                debug(f'Running on {path}'.ljust(max_len + 12), end='\r')

                comp = os.path.join(tmp, filename) + '.huf'
                decomp = os.path.join(tmp, filename) + '.dehuf'

                command = create_command([('--compress', ''), ('--input', path), ('--output', comp)])
                _, rc = run_command(command)
                if rc != 0:
                    continue
                self.assertTrue(os.path.exists(comp), 'Output compressed file not created')

                command = create_command([('--decompress', ''), ('--input', comp), ('--output', decomp)])
                _, rc = run_command(command)
                self.assertEqual(rc, 0, 'Return code of decompress is not zero, while compress succeeded')
                self.assertTrue(os.path.exists(decomp), 'Output decompressed file not created')

                # self.assertEqual(file_checksum(path), file_checksum(decomp), 'Checksum not matched')
                self.batch_compare(path, decomp)

                os.remove(comp)
                os.remove(decomp)
        debug()

    def test_source(self):
        debug('\nTesting source dir')
        self.run_dir(os.path.abspath(os.getcwd()))

    def test_tmp(self):
        if CI_RUN:
            self.skipTest('Temp dir is not good for testing')
        debug('\nTesting temp dir, can fail sometimes :)')
        self.run_dir(tempfile.gettempdir())

    def test_usr(self):
        if not CI_RUN:
            self.skipTest('Your /usr/local is probably very big, to enable run with CI_RUN environment variable')
        debug('\nTesting /usr/local dir')
        self.run_dir('/usr/local', random_help=0.9)


if __name__ == '__main__':
    unittest.main()
