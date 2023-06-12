import os

repet_count = int(input("Enter number of repetitions: "))

print("Run with default schedule:")

os.system("g++ -o omp4 -fopenmp hard.cpp")
for i in range(-1, 11):
    avg = 0
    for j in range(repet_count):
        string = os.popen("./omp4 " + str(i) + " in.pgm out.pgm").read()
        avg += float(string.split(":")[1].split()[0])
    avg = avg / repet_count
    print("Number of threads:", i, "Time:", avg)

print("Run with static schedule:")
for i in range(1, 102, 10):
    avg = 0
    os.system("g++ -o omp4 -DSCHEDULE_PARAMS=static -DSCHEDULE_CHUNK_COUNT=" + str(i) + " -fopenmp hard.cpp")
    for j in range(repet_count):
        string = os.popen("./omp4 4 in.pgm out.pgm").read()
        avg += float(string.split(":")[1].split()[0])
    avg = avg / repet_count
    print("Chunk size:", i, "Time:", avg)

print("Run with dynamic schedule:")
for i in range(1, 102, 10):
    avg = 0
    os.system("g++ -o omp4 -DSCHEDULE_PARAMS=dynamic -DSCHEDULE_CHUNK_COUNT=" + str(i) + " -fopenmp hard.cpp")
    for j in range(repet_count):
        string = os.popen("./omp4 4 in.pgm out.pgm").read()
        avg += float(string.split(":")[1].split()[0])
    avg = avg / repet_count
    print("Chunk size:", i, "Time:", avg)

print("Run with guided schedule:")
for i in range(1, 102, 10):
    avg = 0
    os.system("g++ -o omp4 -DSCHEDULE_PARAMS=guided -DSCHEDULE_CHUNK_COUNT=" + str(i) + " -fopenmp hard.cpp")
    for j in range(repet_count):
        string = os.popen("./omp4 4 in.pgm out.pgm").read()
        avg += float(string.split(":")[1].split()[0])
    avg = avg / repet_count
    print("Chunk size:", i, "Time:", avg)