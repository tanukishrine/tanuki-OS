import sys

LINE_SIZE = 64

def pad_file(input_path, output_path):
	with open(input_path, "rb") as input_file, open(output_path, "wb") as output_file:
		line_number = 0

		for raw_line in input_file:
			line_number += 1

			# remove trailing newline characters
			line = raw_line.rstrip(b"\r\n")

			if len(line) > LINE_SIZE:
				raise ValueError(
					f"Error on line {line_number}: "
					f"line is {len(line)} bytes (max {LINE_SIZE})"
				)

			# pad with 0x20
			padded = line.ljust(LINE_SIZE, b" ")

			# write 64 bytes, no new lines
			output_file.write(padded)

if __name__ == "__main__":
	if len(sys.argv) != 3:
		print(f"Usage: {sys.argv[0]} <input.txt> <output.bin>")
		sys.exit(1)

	try:
		pad_file(sys.argv[1], sys.argv[2])
	except Exception as e:
		print(e, file=sys.stderr)
		sys.exit(1)
