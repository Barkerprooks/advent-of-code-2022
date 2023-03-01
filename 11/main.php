<?php

function clip_at_first($char, $line) {
	return substr($line, strpos($line, $char) + strlen($char));
}

function parse_section($section) {
	$lines = explode("\n", $section);
	return [
		"items" => explode(", ", clip_at_first(": ", $lines[1])),
		"expr" => clip_at_first("new = ", $lines[2]),
		"test" => (int) clip_at_first("by ", $lines[3]),
		"cond" => [
			true => clip_at_first("monkey ", $lines[4]),
			false => clip_at_first("monkey ", $lines[5])
		]
	];
}

function parse_file($path) {
	
	$file = fopen($path, "r");
	$size = fstat($file)["size"];
	$text = fread($file, $size);

	$monkeys = array_map("parse_section", explode("\n\n", $text));

	fclose($file);

	return $monkeys;
}

$monkeys = parse_file("./input.txt");

foreach ($monkeys as $monkey) {
	foreach ($monkey["items"] as $item) {
		
		$expr = "echo ".str_replace("old", $item, $monkey["expr"]).";";
		$worry = (int) eval($expr);

		$test = $worry % $monkey["test"] == 0;
		$worry = $test ? $worry : $worry / 3;
		$index = $monkey["cond"][$test];

		$monkeys[$index]["items"] = array_push($monkeys[$index]["items"], $worry);
	}
}

foreach ($monkeys as $monkey)
	echo implode(", ", $monkey["items"]);

?>
