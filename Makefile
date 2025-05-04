foo: foo.o
	ld -macos_version_min 15.0.0 -o foo foo.o -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path` -arch arm64 

foo.o: foo.s
	as -o foo.o foo.s

clean:
	rm foo foo.o
