default:
	@echo "Please Specify a target: serial, OMP, MPI, or clean"
	@exit 1
serial: serial.f95
	gfortran -Wuninitialized -static-libgfortran -I/home/eos/.local/include/ -L/home/eos/.local/lib/ -O3 -march=native -o serial serial.f95 -lfm

debug:
	gfortran -g -Wuninitialized -static-libgfortran -I/home/eos/.local/include/ -L/home/eos/.local/lib/  -o serial serial.f95 -lfm
clean:
	rm -rf serial
