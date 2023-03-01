import java.io.*;
import java.util.Scanner;
import java.util.Vector;

public class Main {

	static Instruction[] parseInput(String path) {	
		
		Vector<Instruction> instructions = new Vector<Instruction>();

		try {
			File file = new File(path);
			Scanner scanner = new Scanner(file);
			while (scanner.hasNextLine()) {
				String[] tokens = scanner.nextLine().split(" ");
				switch (tokens.length) {
					case 1 -> instructions.add(new Instruction(tokens.length, 0)); 
					case 2 -> instructions.add(new Instruction(tokens.length, Integer.parseInt(tokens[1])));
				}
			}
		} catch (FileNotFoundException exception) {
			System.out.println(exception);
		}

		return instructions.toArray(new Instruction[instructions.size()]);
	}

	public static String drawBuffer(int value) {
		
		char[] buffer = new char[40];

		for (int i = 0; i < buffer.length; i++)
			buffer[i] = '.';


		for (int i = value; i < buffer.length && i < value + 3; i++) {
			if (i >= 0 && i < 40)
				buffer[i] = '#';
		}

		return new String(buffer);
	}

	public static void main(String args[]) {
		
		int i, total = 0, cycle = 0, value = 1;
		char[] screen = new char[40 * 6];
		String buffer = drawBuffer(0);

		for (i = 0; i < screen.length; i++)
			screen[i] = '.';

		for (Instruction instruction : parseInput("./input.txt")) {
			while (instruction.step()) {

				screen[cycle] = buffer.charAt(cycle % 40);	
				if (cycle % 40 < 6)
					System.out.printf("%d:%d, %s\n", cycle % 40, cycle, buffer);

				cycle++;
				switch (cycle) {
					case 20:
					case 60:
					case 100:
					case 140:
					case 180:
					case 220:
						total += value * cycle;
						break;
				}
			}
			value += instruction.getValue();
			buffer = drawBuffer(value - 1);
		}

		System.out.println(String.format("p1: %d\n", total));
		for (i = 0; i < screen.length; i++) {
			if (i % 40 == 0)
				System.out.println();
			System.out.print(screen[i]);
		}
		System.out.println();
	}
}
	
class Instruction {
	
	private int cycles = 0;
	private int output = 0;

	public Instruction(int cycles, int output) {
		this.cycles = cycles;
		this.output = output;
	}

	public boolean step() {
		if (this.cycles > 0) {
			this.cycles--;
			return true;
		}
		return false;
	}

	public Integer getValue() {
		if (this.cycles != 0)
			return null;
		return this.output;
	}
}
