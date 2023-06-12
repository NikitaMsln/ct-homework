#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

const char *REGISTER_NAMES[] = {
	"zero",
	"ra",
	"sp",
	"gp",
	"tp",
	"t0",
	"t1",
	"t2",
	"fp",
	"s1",
	"a0",
	"a1",
	"a2",
	"a3",
	"a4",
	"a5",
	"a6",
	"a7",
	"s2",
	"s3",
	"s4",
	"s5",
	"s6",
	"s7",
	"s8",
	"s9",
	"s10",
	"s11",
	"t3",
	"t4",
	"t5",
	"t6"
};

const char *OP_NAMES_I_TYPE[] = {
	"addi",
	"slli",
	"slti",
	"sltiu",
	"xori",
	NULL,
	"ori",
	"andi"
};

const char *OP_NAMES_RI_TYPE[] = {
	NULL,
	"sll",
	"slt",
	"sltu",
	"xor",
	NULL,
	"or",
	"and"
};

const char *OP_NAMES_RM_TYPE[] = {
	"mul",
	"mulh",
	"mulhsu",
	"mulhu",
	"div",
	"divu",
	"rem",
	"remu"
};

const char *OP_NAMES_CSR_TYPE[] = {
	NULL,
	"csrrw",
	"csrrs",
	"csrrc",
	NULL,
	"csrrwi",
	"csrrsi",
	"csrrci"
};

const char *OP_NAMES_LOAD_TYPE[] = {
	"lb",
	"lh",
	"lw",
	NULL,
	"lbu",
	"lhu"
};

const char *OP_NAMES_STORE_TYPE[] = {
	"sb",
	"sh",
	"sw"
};

const char *OP_NAMES_BJUMP_TYPE[] = {
	"beq",
	"bne",
	NULL,
	NULL,
	"blt",
	"bge",
	"bltu",
	"bgeu"
};

const char *SYMTAB_TYPES[] = {
	"NOTYPE",
	"OBJECT",
	"FUNC",
	"SECTION",
	"FILE",
	"COMMON",
	"TLS",
	NULL,
	NULL,
	NULL,
	"LOOS",
	NULL,
	"HIOS",
	"LOPROC",
	NULL,
	"HIPROC"
};

const char *SYMTAB_BINDES[] = {
	"LOCAL",
	"GLOBAL",
	"WEAK",
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	"LOOS",
	NULL,
	"HIOS",
	"LOPROC",
	NULL,
	"HIPROC"
};

const char *SYMTAB_VISIBILITIES[] = {
	"DEFAULT",
	"INTERNAL",
	"HIDDEN",
	"PROTECTED"
};

int parse_header(FILE *in, int position, uint32_t *result);

int parse_section(FILE *in, int position, uint32_t *result);

int read_string(FILE *in, int position, int size, char* result);

int print_symtab(FILE *in, FILE *out, uint32_t **symtab_data, int symtab_size,
	         int names_section_position, int names_section_size);

int parse_symtab(FILE *in, int position, int size, uint32_t **symtab);

int parse_commands(FILE *in, FILE *out, int position, int size, int virtual_start,
	int* named_labels, char **label_names, int label_count);

void set_value_in_little_endian(uint32_t *result, uint8_t *source, int size);

uint32_t get_bits(uint32_t src, int pos, int count, int result_shift);

int main(int argc, char *argv[]) {

	if (argc < 3)
	{
		printf("Have no input or output filenames\n");
		return 1;
	}

	FILE *in, *out;

	if ((in = fopen(argv[1], "rb")) == NULL) {
		printf("Cannot open input file: %s\n", argv[1]);
		return 2;
	}

	if ((out = fopen(argv[2], "wt")) == NULL) {
		printf("Cannot open output file: %s\n", argv[2]);
		return 2;
	}

	uint32_t header_data[29];

	int error = parse_header(in, 0, header_data);
	if (error == 3) {
		printf("Unsupported elf type file\n");
		return 3;
	} else if (error == 2) {
		printf("Invalid elf file\n");
		return 3;
	} else if (error == 1) {
		printf("Reading input file error\n");
		return 2;
	}

	int section_count = header_data[27];
	int section_names_index = header_data[28];

	uint32_t **section_data = (uint32_t **)malloc(section_count * sizeof(uint32_t *));

	int text_section = -1, symtab_section = -1;

	for (int i = 0; i < section_count; i++) {
		section_data[i] = (uint32_t *)malloc(10 * sizeof(uint32_t));
		if (parse_section(in, header_data[21] + 40 * i, section_data[i])) {
			printf("Reading input file error\n");
			for (int j = i - 1; j>= 0; j--)
				free(section_data[j]);
			free(section_data);
			return 2;
		}

		if (section_data[i][1] == 2) { // SHT_SYMTAB
			symtab_section = i;
		}

		if (section_data[i][1] == 1 && ((section_data[i][2] >> 1) & 0x3) == 0x3) { // SHT_PROGBITS, SHF_ALLOC, SHF_EXECINSTR
			text_section = i;
		}
	}

	if (text_section < 0) {
		printf(".text section not found\n");
		for (int i = 0; i < section_count; i++)
			free(section_data[i]);
		free(section_data);
		return 3;
	}

	if (symtab_section < 0) {
		printf(".symtab section not found\n");
		for (int i = 0; i < section_count; i++)
			free(section_data[i]);
		free(section_data);
		return 3;
	}

	uint32_t name_pointer = section_data[text_section][0] + section_data[section_names_index][4],
		 name_max_size = section_data[section_names_index][5] - section_data[text_section][0];

	int symtab_size = section_data[symtab_section][5] / 16; // symtab size / size of 1 symbol

	uint32_t **symtab_data = (uint32_t **)malloc(symtab_size * sizeof(uint32_t *));
	for (int i = 0; i < symtab_size; i++) {
		symtab_data[i] = (uint32_t *)malloc(6 * sizeof(uint32_t));
	}

	if (parse_symtab(in, section_data[symtab_section][4], symtab_size, symtab_data)) {
		printf("Reading input file error\n");
		for (int i = 0; i < section_count; i++)
			free(section_data[i]);
		free(section_data);
		for (int i = 0; i < symtab_size; i++)
			free(symtab_data[i]);
		free(symtab_data);
		return 2;
	}

	int label_count = 0;
	int *label_addresses = (int *)malloc(symtab_size * sizeof(int));
	char **label_names = (char **)malloc(symtab_size * sizeof(char *));

	for (int i = 0; i < symtab_size; i++) {
		if (symtab_data[i][5] == text_section) { // st_shndx is .text index
			label_addresses[label_count] = symtab_data[i][1];

			int name_size = section_data[section_data[symtab_section][7]][5] - symtab_data[i][0]; // size of section with names - start of name
			label_names[label_count] = (char *)malloc(name_size * sizeof(char));
			read_string(
				in,
				section_data[section_data[symtab_section][7]][4] + symtab_data[i][0],
				name_size,
				label_names[label_count]
			);

			label_count++;
		}
	}

	char *string = (char *)malloc(name_max_size * sizeof(char));

	if (read_string(in, name_pointer, name_max_size, string) == 0) {
		printf("Reading input file error\n");
	}

	if (fprintf(out, "%s\n", string) < 0) {
		printf("Writing in output file error\n");
	}
	free(string);

	int parsing_commands_result = parse_commands(
			in,
			out,
			section_data[text_section][4], // position
			section_data[text_section][5], // size
			header_data[19], // virtual start
			label_addresses,
			label_names,
			label_count
	);

	for (int i = 0; i < label_count; i++) {
		free(label_names[i]);
	}
	free(label_names);
	free(label_addresses);

	if (parsing_commands_result != 0) {
		for (int i = 0; i < section_count; i++)
			free(section_data[i]);
		free(section_data);
		for (int i = 0; i < symtab_size; i++)
			free(symtab_data[i]);
		free(symtab_data);
	}

	if (parsing_commands_result == 1) {
		printf("Reading section .text error\n");
		return 2;
	} else if (parsing_commands_result == 2) {
		printf("Parsing section .text error (section may be incorrect or unsupported)\n");
	} else if (parsing_commands_result == 3) {
		printf("Writing in output file error\n");
		return 2;
	}



	name_pointer = section_data[symtab_section][0] + section_data[section_names_index][4];
	name_max_size = section_data[section_names_index][5] - section_data[symtab_section][0];

	string = (char *)malloc(name_max_size * sizeof(char));

	if (read_string(in, name_pointer, name_max_size, string) == 0) {
		printf("Reading input file error\n");
	}

	if (fprintf(out, "\n%s\n", string) < 0) {
		printf("Writing in output file error\n");
	}

	free(string);

	int printing_symtab_result = print_symtab(
		in, out, symtab_data, symtab_size,
		section_data[section_data[symtab_section][7]][4],
		section_data[section_data[symtab_section][7]][5]
	);

	for (int i = 0; i < section_count; i++)
		free(section_data[i]);
	free(section_data);
	for (int i = 0; i < symtab_size; i++)
		free(symtab_data[i]);
	free(symtab_data);

	if (printing_symtab_result) {
		printf("Writing in output file error\n");
		return 2;
	}

	if (fclose(in)) {
		printf("Cannot close input file: %s\n", argv[1]);
		return 2;
	}

	if (fclose(out)) {
		printf("Cannot close output file: %s\n", argv[2]);
		return 2;
	}

	return 0;
}

int parse_commands(FILE *in, FILE *out, int position, int size, int virtual_start,
	int* named_labels, char **label_names, int label_count) {

	if (fseek(in, position, SEEK_SET))
		return 1;

	uint32_t *commands = (uint32_t *)malloc(size / 4 * sizeof(uint32_t));
	int *labels = (int *)malloc(size / 4 * sizeof(int));

	for (int i = 0; i < size / 4; i++)
		labels[i] = 0;			// 0 - no label; < 0 - label from symtab; > 0 - label L*

	for (int i = 0; i < label_count; i++) {
		if (0 <= named_labels[i] - virtual_start && named_labels[i] - virtual_start < size) {
			labels[(named_labels[i] - virtual_start) / 4] = -i - 1;
		}
	}

	uint8_t buff[4096];

	int unnamed_label_number = 0;

	for (int i = 0, j = 0, readed = 0; i * 4 < size; i++) {
		commands[i] = 0;

		for (int k = 0; k < 4; k++, j++) {
			if (j >= readed) {
				j = 0;
				readed = fread(buff, sizeof(uint8_t), 4096, in);

				if (readed == 0) {
					free(commands);
					free(labels);
					return 1;
				}
			}

			commands[i] += buff[j] << (k * 8);
		}

		int op_code = commands[i] & 0x7f;
		uint32_t address = 0;
		int have_address = 0;
		int index = -1;

		if (op_code == 0x6f) {
			address = get_bits(commands[i], 31, 1, 19) +
				  get_bits(commands[i], 30, 10, 9) +
				  get_bits(commands[i], 27, 1, 10) +
				  get_bits(commands[i], 19, 8, 18);
			if ((address >> 19 & 1) == 1) {
				address = 0x7ffff - (address >> 1) + 1;
				address = virtual_start + 4 * i - (address << 2);
			} else {
				address = virtual_start + 4 * i + (address << 1);
			}
			have_address = 1;
		} else if (op_code == 0x63) {
			address = get_bits(commands[i], 31, 1, 11) +
				  get_bits(commands[i], 30, 6, 9) +
				  get_bits(commands[i], 11, 4, 3) +
				  get_bits(commands[i], 7, 1, 10);
			if ((address >> 11 & 1) == 1) {
				address = 0x7ff - (address >> 1) + 1;
				address = virtual_start + 4 * i - (address << 2);
			} else {
				address = virtual_start + 4 * i + (address << 1);
			}
			have_address = 1;
		}

		if (have_address) index = (address - virtual_start) / 4;

		if (0 <= index && index < size / 4 && labels[index] == 0) {
			labels[index] = ++unnamed_label_number;
		}
	}

	int result_of_parsing = 0;

	for (int i = 0; i * 4 < size && result_of_parsing != 3; i++) {
		if (labels[i] < 0) {
			int index = -labels[i] - 1;
			if (fprintf(out, "%08x   <%s>:\n", named_labels[index], label_names[index]) < 0)
				result_of_parsing = 3;
		} else if (labels[i] > 0) {
			if (fprintf(out, "%08x   <L%d>:\n", virtual_start + 4 * i, labels[i] - 1) < 0)
				result_of_parsing = 3;
		}

		if (fprintf(out, "   %05x:\t%08x\t", virtual_start + 4 * i, commands[i]) < 0) result_of_parsing = 3;

		int op_code = commands[i] & 0x7f;

		if (op_code == 0x37 || op_code == 0x17) {
			if (fprintf(out, "%7s\t", (op_code == 0x37) ? "lui" : "auipc") < 0) result_of_parsing = 3;
			if (fprintf(
				out,
				"%s, 0x%x\n",
				REGISTER_NAMES[(commands[i] >> 7) & 0x1f],
				commands[i] >> 12
			) < 0) result_of_parsing = 3;
		} else if (op_code == 0x13) {
			int op_type = (commands[i] >> 12) & 0x7;
			if (op_type != 0x1 && op_type != 0x5) {
				int imm = commands[i] >> 20;
				if (imm >> 11)
					imm = imm - 0x1000;
				if (fprintf(out, "%7s\t%s, %s, %d\n",
					OP_NAMES_I_TYPE[op_type],
					REGISTER_NAMES[(commands[i] >> 7) & 0x1f],
					REGISTER_NAMES[(commands[i] >> 15) & 0x1f],
					imm
				) < 0) result_of_parsing = 3;
			} else {
				if (op_type == 0x1) {
					if (fprintf(out, "%7s\t", OP_NAMES_I_TYPE[op_type]) < 0) result_of_parsing = 3;
				} else {
					if (fprintf(out, "%7s\t", (commands[i] >> 30) & 1? "srai" : "srli") < 0)
						result_of_parsing = 3;
				}

				if (fprintf(out, "%s, %s, %d\n",
					REGISTER_NAMES[(commands[i] >> 7) & 0x1f],
					REGISTER_NAMES[(commands[i] >> 15) & 0x1f],
					(commands[i] >> 20) & 0x1f
				) < 0) result_of_parsing = 3;
			}
		} else if (op_code == 0x33) {
			int op_type = (commands[i] >> 12) & 0x7;
			if ((commands[i] >> 25) & 0x1) {
				if (fprintf(out, "%7s\t%s, %s, %s\n",
					OP_NAMES_RM_TYPE[op_type],
					REGISTER_NAMES[(commands[i] >> 7) & 0x1f],
					REGISTER_NAMES[(commands[i] >> 15) & 0x1f],
					REGISTER_NAMES[(commands[i] >> 20) & 0x1f]
				) < 0) result_of_parsing = 3;
			} else {
				if (op_type == 0x0) {
					if (fprintf(out, "%7s\t", (commands[i] >> 30)? "sub" : "add") < 0)
						result_of_parsing = 3;
				} else if (op_type == 0x5) {
					if (fprintf(out, "%7s\t", (commands[i] >> 30)? "sra" : "srl") < 0)
					        result_of_parsing = 3;
				} else {
					if (fprintf(out, "%7s\t", OP_NAMES_RI_TYPE[op_type]) < 0)
					        result_of_parsing = 3;
				}

				if (fprintf(out, "%s, %s, %s\n",
					REGISTER_NAMES[(commands[i] >> 7) & 0x1f],
					REGISTER_NAMES[(commands[i] >> 15) & 0x1f],
					REGISTER_NAMES[(commands[i] >> 20) & 0x1f]
				) < 0) result_of_parsing = 3;
			}
		} else if (op_code == 0xf) {
			if ((commands[i] >> 12) & 1) {
				if (fprintf(out, "fence.i\n") < 0) result_of_parsing = 3;
			} else {
				char pred[5] = {0, 0, 0, 0, 0}, succ[5] = {0, 0, 0, 0, 0};
				int pk = 0, ps = 0;
				if ((commands[i] >> 27) & 1)
					pred[pk++] = 'i';
				if ((commands[i] >> 26) & 1)
					pred[pk++] = 'o';
				if ((commands[i] >> 25) & 1)
					pred[pk++] = 'r';
				if ((commands[i] >> 24) & 1)
					pred[pk++] = 'w';

				if ((commands[i] >> 23) & 1)
					succ[ps++] = 'i';
				if ((commands[i] >> 22) & 1)
					succ[ps++] = 'o';
				if ((commands[i] >> 21) & 1)
					succ[ps++] = 'r';
				if ((commands[i] >> 20) & 1)
					succ[ps++] = 'w';
				if (fprintf(out, "  fence\t%s, %s\n", pred, succ) < 0) result_of_parsing = 3;
			}
		} else if (op_code == 0x73) {
			int op_type = (commands[i] >> 12) & 0x7;
			if (op_type == 0x0) {
				if (commands[i] >> 20) {
					if (fprintf(out, " ebreak\n") < 0) result_of_parsing = 3;
				} else {
					if (fprintf(out, "  ecall\n") < 0) result_of_parsing = 3;
				}
			} else if (op_type != 0x4) {
				if (fprintf(out, "%7s\t%s, %x, ",
					OP_NAMES_CSR_TYPE[op_type],
					REGISTER_NAMES[(commands[i] >> 7) & 0xf],
					commands[i] >> 20
				) < 0) result_of_parsing = 3;
				if (op_type >> 2) {
					if (fprintf(out, "%d\n", (commands[i] >> 15) & 0x1f) < 0) result_of_parsing = 3;
				} else {
					if (fprintf(out, "%s\n", REGISTER_NAMES[(commands[i] >> 15) & 0x1f]) < 0)
						result_of_parsing = 3;
				}
			} else {
				result_of_parsing = 2;
			}
		} else if (op_code == 0x3) {
			if (fprintf(out, "%7s\t%s, %d(%s)\n",
				OP_NAMES_LOAD_TYPE[(commands[i] >> 12) & 0x7],
				REGISTER_NAMES[(commands[i] >> 7) & 0x1f],
				commands[i] >> 20,
				REGISTER_NAMES[(commands[i] >> 15) & 0x1f]
			) < 0) result_of_parsing = 3;
		} else if (op_code == 0x23) {
			if (fprintf(out, "%7s\t%s, %d(%s)\n",
				OP_NAMES_STORE_TYPE[(commands[i] >> 12) & 0x7],
				REGISTER_NAMES[(commands[i] >> 20) & 0x1f],
				(((commands[i] >> 25) & 0x7f) << 5) + ((commands[i] >> 7) & 0x1f),
				REGISTER_NAMES[(commands[i] >> 15) & 0x1f]
			) < 0) result_of_parsing = 3;
		} else if (op_code == 0x6f) {
			int address = get_bits(commands[i], 31, 1, 19) +
				      get_bits(commands[i], 30, 10, 9) +
				      get_bits(commands[i], 27, 1, 10) +
				      get_bits(commands[i], 19, 8, 18);
			if ((address >> 19 & 1) == 1) {
				address = 0x7ffff - (address >> 1) + 1;
				address = virtual_start + 4 * i - (address << 2);
			} else {
				address = virtual_start + 4 * i + (address << 1);
			}
			int index = (address - virtual_start) / 4;
			if (fprintf(out, "    jal\t%s, 0x%x",
				REGISTER_NAMES[(commands[i] >> 7) & 0x1f],
				address
			) < 0) result_of_parsing = 3;
			if (labels[index] < 0) {
				if (fprintf(out, " <%s>", label_names[-labels[index] - 1]) < 0) result_of_parsing = 3;
			} else if (labels[index] > 0) {
				if (fprintf(out, " <L%d>", labels[index] - 1) < 0) result_of_parsing = 3;
			} else {
				result_of_parsing = 2;
			}
			if (fprintf(out, "\n") < 0) result_of_parsing = 3;
		} else if (op_code == 0x67) {
			if (fprintf(out, "   jalr\t%s, 0x%x(%s)\n",
				REGISTER_NAMES[(commands[i] >> 7) & 0x1f],
				commands[i] >> 20,
				REGISTER_NAMES[(commands[i] >> 15) & 0x1f]
			) < 0) result_of_parsing = 3;
		} else if (op_code == 0x63) {
			int address = get_bits(commands[i], 31, 1, 11) +
				  get_bits(commands[i], 30, 6, 9) +
				  get_bits(commands[i], 11, 4, 3) +
				  get_bits(commands[i], 7, 1, 10);
			if ((address >> 11 & 1) == 1) {
				address = 0x7ff - (address >> 1) + 1;
				address = virtual_start + 4 * i - (address << 2);
			} else {
				address = virtual_start + 4 * i + (address << 1);
			}
			int index = (address - virtual_start) / 4;
			if (fprintf(out, "%7s\t%s, %s, 0x%x",
				OP_NAMES_BJUMP_TYPE[(commands[i] >> 12) & 0x7],
				REGISTER_NAMES[(commands[i] >> 15) & 0x1f],
				REGISTER_NAMES[(commands[i] >> 20) & 0x1f],
				address
			) < 0) result_of_parsing = 3;
			if (labels[index] < 0) {
				if (fprintf(out, " <%s>", label_names[-labels[index] - 1]) < 0) result_of_parsing = 3;
			} else if (labels[index] > 0) {
				if (fprintf(out, " <L%x>", labels[index] - 1) < 0) result_of_parsing = 3;
			} else {
				result_of_parsing = 2;
			}
			if (fprintf(out, "\n") < 0) result_of_parsing = 3;
		} else {
			fprintf(out, "unknown_instruction\n");
			result_of_parsing = 2;
		}
	}

	free(commands);
	free(labels);
	return result_of_parsing;
}

int print_symtab(FILE *in, FILE *out, uint32_t **symtab_data, int symtab_size,
	       	 int names_section_position, int names_section_size) {

	char **names = (char **)malloc(symtab_size * sizeof(char *));

	for (int i = 0; i < symtab_size; i++) {
		int name_size = names_section_size - symtab_data[i][0];

		if (symtab_data[i][0] > 0) {
			names[i] = (char *)malloc(name_size * sizeof(char));
			int size = read_string(
				in,
				names_section_position + symtab_data[i][0],
				name_size,
				names[i]
			);
			if (size == 0) names[i] = NULL;
		} else {
			names[i] = NULL;
		}
	}


	int result_of_printing = 0;
	if (fprintf(out, "Symbol Value          	Size Type 	Bind 	Vis   	Index Name\n") < 0)
	       result_of_printing = 3;
	for (int i = 0; i < symtab_size && result_of_printing == 0; i++) {
		char *index;
		int is_spec_index = 0;
		if (symtab_data[i][5] == 0) {
			index = "UNDEF";
			is_spec_index = 1;
		} else if ((symtab_data[i][5] >> 8) == 0xff) {
			int type = symtab_data[i][5] & 0xff;
			is_spec_index = 1;
			if (type == 0) {
				index = "LOPROC";
			} else if (type == 0x1f) {
				index = "HIPROC";
			} else if (type == 0x20) {
				index = "LOOS";
			} else if (type == 0x3f) {
				index = "HIOS";
			} else if (type == 0xf1) {
				index = "ABS";
			} else if (type == 0xf2) {
				index = "COMMON";
			} else if (type == 0xff) {
				index = "XINDEX";
			} else {
				is_spec_index = 0;
			}
		}

		if (fprintf(out, "[%4i] 0x%-15X %5i %-8s %-8s %-8s ",
			i,
			symtab_data[i][1], // value
			symtab_data[i][2], // size
			SYMTAB_TYPES[symtab_data[i][3] & 0xf],
			SYMTAB_BINDES[(symtab_data[i][3] >> 4) & 0xf],
			SYMTAB_VISIBILITIES[symtab_data[i][4] & 0x3]
		) < 0) result_of_printing = 3;

		if (is_spec_index) {
			if (fprintf(out, "%6s ", index) < 0) result_of_printing = 3;
		} else {
			if (fprintf(out, "%6i ", symtab_data[i][5]) < 0) result_of_printing = 3;
		}
		if (fprintf(
			out,
			"%s\n",
			(names[i] != NULL)? names[i] : "\0"
		) < 0) result_of_printing = 3;
	}

	for (int i = 0; i < symtab_size; i++) {
		if (names[i] != NULL) free(names[i]);
	}
	free(names);

	return result_of_printing;
}

int parse_symtab(FILE *in, int position, int size, uint32_t **symtab) {

	if (fseek(in, position, SEEK_SET))
		return 1;

	for (int i = 0; i < size; i++) {
		uint8_t buff[16];
		if (fread(buff, sizeof(uint8_t), 16, in) < 16)
			return 1;

		set_value_in_little_endian(symtab[i], buff, 4);			// Read st_name
		set_value_in_little_endian(symtab[i] + 1, buff + 4, 4);	// Read st_value
		set_value_in_little_endian(symtab[i] + 2, buff + 8, 4); // Read st_size
		symtab[i][3] = buff[12];								// Read st_info
		symtab[i][4] = buff[13];								// Read st_other
		set_value_in_little_endian(symtab[i] + 5, buff + 14, 2);// Read st_shndx
	}

	return 0;
}

int read_string(FILE *in, int position, int size, char* result) {

	if (fseek(in, position, SEEK_SET))
		return 0;

	uint8_t buff[4096];
	int i = 0, j = 0, readed = 0;

	for (; i < size; i++, j++) {
		if (j >= readed) {
			j = 0;
			readed = fread(buff, sizeof(uint8_t), 4096, in);

			if (readed == 0)
				return i;
		}
		result[i] = (char)buff[j];
		if (buff[i] == 0)
			return i;
	}

	return i;
}

int parse_section(FILE *in, int position, uint32_t *result) {

	if (fseek(in, position, SEEK_SET))
		return 1;

	uint8_t buff[40];
	if (fread(buff, sizeof(uint8_t), 40, in) < 40)
		return 1;

	for (int i = 0, k = 0; i < 10; i++, k += 4) {
		set_value_in_little_endian(result + i, buff + k, 4);
	}
	return 0;
}

int parse_header(FILE *in, int position, uint32_t *result) {

	if (fseek(in, position, SEEK_SET))
		return 1;

	uint8_t buff[64];
	if (fread(buff, sizeof(uint8_t), 16, in) < 16)
		return 1;

	int pos = 0;

	for (int i = 0; i < 16; i++, pos++) // Read e_ident
		result[pos] = buff[i];

	if (result[5] != 1) // Not a little endian
		return 3;

	if (result[4] != 1) // Not a 32bit elf file
		return 3;

	if (fread(buff, sizeof(uint8_t), 36, in) < 36)
		return 1;

	set_value_in_little_endian(result + pos++, buff, 2); // Read e_type
	set_value_in_little_endian(result + pos++, buff + 2, 2); // Read e_machine

	int ind = 4;

	for (int k = 0; k < 5; k++, pos++, ind += 4) {
		set_value_in_little_endian(result + pos, buff + ind, 4); // Read e_version, e_entry, e_phoff, e_shoff, e_flags
	}

	for (; ind < 36; ind += 2, pos++) {
		set_value_in_little_endian(result + pos, buff + ind, 2); // Read other data
	}

	if (result[18] != 1) // Incorrect version
		return 2;

	if (result[17] != 0xf3) // Not a RISCV
		return 3;

	if (result[16] != 2) // Not an executable file
		return 3;

	return 0;
}

void set_value_in_little_endian(uint32_t *result, uint8_t *source, int size) {
	*result = 0;
	for (int i = 0; i < size; i++)
		*result += source[i] << (8 * i);
}

uint32_t get_bits(uint32_t src, int pos, int count, int result_shift) {
	uint32_t result = 0;
	for (int i = pos - count + 1, j = result_shift + 1 - count; i <= pos; i++, j++)
	       result += ((src >> i) & 1) << j;
	return result;
}