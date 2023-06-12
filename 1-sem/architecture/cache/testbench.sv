`define CACHE_ADDR_SIZE 20
`define CACHE_DATA_SIZE 16
`define CACHE_SET_SIZE 4
`define CACHE_OFFSET_SIZE 5
`define CACHE_TAG_SIZE 11

`define CACHE_WAY 2
`define CACHE_SETS_COUNT 16
`define CACHE_LINE_COUNT 32
`define CACHE_TAG_COUNT 2048

`define VALID 0
`define DIRTY 1

`define C1_NOP 0
`define C1_READ8 1
`define C1_READ16 2
`define C1_READ32 3
`define C1_INVALIDATE_LINE 4
`define C1_WRITE8 5
`define C1_WRITE16 6
`define C1_WRITE32 7
`define C1_RESPONSE 7

`define C2_NOP 0
`define C2_READ_LINE 1
`define C2_WRITE_LINE 2
`define C2_RESPONSE 3


module Cache(
	input clk, c_dump, reset,
	input[0:(`CACHE_TAG_SIZE + `CACHE_SET_SIZE - 1)] a1, inout[0:(`CACHE_DATA_SIZE - 1)] d1, inout[0:2] c1,
	output[0:(`CACHE_TAG_SIZE + `CACHE_SET_SIZE - 1)] a2, inout[0:(`CACHE_DATA_SIZE - 1)] d2, inout[0:1] c2
);
	reg [0:(`CACHE_DATA_SIZE - 1)] d1out = 0;
	reg [0:2] c1out = `C1_NOP;
	reg [0:(`CACHE_DATA_SIZE - 1)] d2out = 0;
	reg [0:1] c2out = `C2_NOP;
	reg [0:(`CACHE_TAG_SIZE + `CACHE_SET_SIZE - 1)] a2out = 0;

	reg writeCPU = 0, writeMEM = 1;

	assign d1 = writeCPU? d1out : {`CACHE_DATA_SIZE{1'bz}};
	assign c1 = writeCPU? c1out : 3'bzzz;
	assign d2 = writeMEM? d2out : {`CACHE_DATA_SIZE{1'bz}};
	assign c2 = writeMEM? c2out : 3'bzz;
	assign a2 = a2out;

	reg [0:7] data [0:(`CACHE_WAY - 1)][0:(`CACHE_SETS_COUNT - 1)][0:(`CACHE_LINE_COUNT - 1)];
	reg [0:(`CACHE_TAG_SIZE - 1)] tags [0:(`CACHE_WAY - 1)][0:(`CACHE_SETS_COUNT - 1)];
	reg [0:1] flags [0:(`CACHE_WAY - 1)][0:(`CACHE_SETS_COUNT - 1)];
	reg farWay [0:(`CACHE_SETS_COUNT - 1)];

	reg [0:31] indata;
	reg [0:(`CACHE_TAG_SIZE - 1)] tag;
	reg [0:(`CACHE_SET_SIZE - 1)] set;
	reg [0:(`CACHE_OFFSET_SIZE - 1)] offset;
	reg waitData;
	reg sendData;
	integer i, j;

	initial begin
		for (i = 0; i < `CACHE_SETS_COUNT; i++) begin
			farWay[i] = 0;
			flags[0][i][`VALID] = 0;
			flags[1][i][`VALID] = 0;
		end
		writeCPU = 0;
		writeMEM = 1;
		waitData = 0;
		sendData = 0;
	end

	always @(posedge reset) begin
		for (i = 0; i < `CACHE_SETS_COUNT; i++) begin
			farWay[i] = 0;
			flags[0][i][`VALID] = 0;
			flags[1][i][`VALID] = 0;
		end
		writeCPU = 0;
		writeMEM = 1;
		waitData = 0;
		sendData = 0;
	end

	always @(posedge c_dump) begin
		$write("[Cache data at %0d]\n", $time);
		for (i = 0; i < `CACHE_SETS_COUNT; i++) begin
			$write("\t%0d: %0b %0b%0b ", i, flags[0][i], tags[0][i], i);
			for (j = 0; j < `CACHE_LINE_COUNT; j++) begin
				$write("%h", data[0][i][j]);
			end
			$write("\n");
		end
		$write("\n");
		for (i = 0; i < `CACHE_SETS_COUNT; i++) begin
			$write("\t%0d: %0b %0b ", i, flags[1][i], tags[1][i]);
			for (j = 0; j < `CACHE_LINE_COUNT; j++) begin
				$write("%h", data[1][i][j]);
			end
			$write("\n");
		end
		$write("[Cache data end]\n");
	end

	always @(posedge waitData) begin
		if (writeMEM === 0) begin
			while (c2 !== `C2_RESPONSE) @(posedge clk);
			for (i = 0; i < `CACHE_LINE_COUNT && !writeMEM; i += 2) begin
				if (i > 0) begin
					@(posedge clk);
					while (c2 !== `C2_RESPONSE) @(posedge clk);
				end
				data[farWay[set]][set][i] = d2[0:7];
				data[farWay[set]][set][i + 1] = d2[8:15];
				if (c_dump === 1) begin
					$write("[Cache get data] [%0d] %0d\n", i, data[farWay[set]][set][i]);
					$write("[Cache get data] [%0d] %0d\n", i+1, data[farWay[set]][set][i + 1]);
				end
			end
			tags[farWay[set]][set] = tag;
			flags[farWay[set]][set][`VALID] = 1;
			flags[farWay[set]][set][`DIRTY] = 0;
			if (c_dump === 1) begin
				$write("[Cache get %b: %b %b ", set, flags[farWay[set]][set], tags[farWay[set]][set]);
				for (j = 0; j < `CACHE_LINE_COUNT; j++) begin
					$write("%h", data[farWay[set]][set][j]);
				end
				$write("]\n");
			end
			waitData = 0;
		end
	end

	always @(posedge sendData) begin
		a2out[0:(`CACHE_TAG_SIZE - 1)] = tags[farWay[set]][set];
		a2out[`CACHE_TAG_SIZE:(`CACHE_SET_SIZE + `CACHE_TAG_SIZE - 1)] = set;
		c2out = `C2_WRITE_LINE;
		for (i = 0; i < `CACHE_LINE_COUNT; i += 2) begin
			d2out[0:7] = data[farWay[set]][set][i];
			d2out[8:15] = data[farWay[set]][set][i+1];
			if (c_dump === 1) begin
				$write("[Cache send data %b] [%0d] %0d\n", set, i, data[farWay[set]][set][i]);
				$write("[Cache send data %b] [%0d] %0d\n", set, i+1, data[farWay[set]][set][i+1]);
			end
			@(posedge clk);
		end
		c2out = `C2_NOP;
		writeMEM = 0;
		while(c2 !== `C2_RESPONSE) @(posedge clk);
		writeMEM = 1;
		sendData = 0;
	end

	always @(posedge clk) begin
		if (writeCPU === 0) begin
			if (c1 === `C1_READ8) begin
				tag = a1[0:(`CACHE_TAG_SIZE - 1)];
				set = a1[`CACHE_TAG_SIZE:(`CACHE_SET_SIZE + `CACHE_TAG_SIZE - 1)];

				@(posedge clk);
				while (c1 !== `C1_READ8) @(posedge clk);
				offset = a1[0:(`CACHE_OFFSET_SIZE - 1)];
				if (flags[0][set][`VALID] === 1 && tags[0][set] === tag) begin
					if (c_dump === 1) begin
						$write("[Cache hit at 0 %b %b %b in READ8]\n", tag, set, offset);
					end
					for (i = 0; i < 5; i++) @(posedge clk); // memory response time simulation
					c1out = `C1_RESPONSE;
					d1out[0:7] = data[0][set][offset];
					writeCPU = 1;
					@(posedge clk);
					writeCPU = 0;
					c1out = `C1_NOP;
					farWay[set] = 1;
				end else if (flags[1][set][`VALID] === 1 && tags[1][set] === tag) begin
					if (c_dump === 1) begin
						$write("[Cache hit at 1 %b %b %b in READ8]\n", tag, set, offset);
					end
					for (i = 0; i < 5; i++) @(posedge clk); // memory response time simulation
					c1out = `C1_RESPONSE;
					d1out[0:7] = data[1][set][offset];
					writeCPU = 1;
					@(posedge clk);
					writeCPU = 0;
					c1out = `C1_NOP;
					farWay[set] = 0;
				end else begin
					if (c_dump === 1) begin
						$write("[Cache miss %b %b %b in READ8]\n", tag, set, offset);
					end
					for (i = 0; i < 3; i++) @(posedge clk); // memory response time simulation
					if (flags[farWay[set]][set][`DIRTY] === 1) begin
						sendData = 1;
						while (sendData === 1) @(posedge clk);
					end

					flags[farWay[set]][set][`VALID] = 0;
					a2out[0:(`CACHE_TAG_SIZE - 1)] = tag;
					a2out[`CACHE_TAG_SIZE:(`CACHE_SET_SIZE + `CACHE_TAG_SIZE - 1)] = set;
					c2out = `C2_READ_LINE;
					@(posedge clk);
					writeMEM = 0;
					c2out = `C2_NOP;
					waitData = 1;
					while(waitData === 1) @(posedge clk);
					writeMEM = 1;

					c1out = `C1_RESPONSE;
					d1out[0:7] = data[farWay[set]][set][offset];
					writeCPU = 1;
					@(posedge clk);
					writeCPU = 0;
					c1out = `C1_NOP;
					farWay[set] = (farWay[set] === 1)? 0 : 1;
				end

			end else if (c1 === `C1_READ16) begin

				tag = a1[0:(`CACHE_TAG_SIZE - 1)];
				set = a1[`CACHE_TAG_SIZE:(`CACHE_SET_SIZE + `CACHE_TAG_SIZE - 1)];
				@(posedge clk);
				while (c1 !== `C1_READ16) @(posedge clk);
				offset = a1[0:(`CACHE_OFFSET_SIZE - 1)];
				if (flags[0][set][`VALID] === 1 && tags[0][set] === tag) begin
					if (c_dump === 1) begin
						$write("[Cache hit at 0 %b %b %b in READ16]\n", tag, set, offset);
					end
					for (i = 0; i < 5; i++) @(posedge clk); // memory response time simulation
					c1out = `C1_RESPONSE;
					d1out[0:7] = data[0][set][offset];
					d1out[8:15] = data[0][set][offset + 1];
					writeCPU = 1;
					@(posedge clk);
					writeCPU = 0;
					c1out = `C1_NOP;
					farWay[set] = 1;
				end else if (flags[1][set][`VALID] === 1 && tags[1][set] === tag) begin
					if (c_dump === 1) begin
						$write("[Cache hit at 1 %b %b %b in READ16]\n", tag, set, offset);
					end
					for (i = 0; i < 5; i++) @(posedge clk); // memory response time simulation
					c1out = `C1_RESPONSE;
					d1out[0:7] = data[1][set][offset];
					d1out[8:15] = data[1][set][offset + 1];
					writeCPU = 1;
					@(posedge clk);
					writeCPU = 0;
					c1out = `C1_NOP;
					farWay[set] = 0;
				end else begin
					if (c_dump === 1) begin
						$write("[Cache miss %b %b %b in READ16]\n", tag, set, offset);
					end
					for (i = 0; i < 3; i++) @(posedge clk); // memory response time simulation
					if (flags[farWay[set]][set][`DIRTY] === 1) begin
						sendData = 1;
						while (sendData === 1) @(posedge clk);
					end

					flags[farWay[set]][set][`VALID] = 0;
					a2out[0:(`CACHE_TAG_SIZE - 1)] = tag;
					a2out[`CACHE_TAG_SIZE:(`CACHE_SET_SIZE + `CACHE_TAG_SIZE - 1)] = set;
					c2out = `C2_READ_LINE;
					@(posedge clk);
					writeMEM = 0;
					c2out = `C2_NOP;
					waitData = 1;
					while(waitData === 1) @(posedge clk);
					writeMEM = 1;

					c1out = `C1_RESPONSE;
					d1out[0:7] = data[farWay[set]][set][offset];
					d1out[8:15] = data[farWay[set]][set][offset + 1];
					writeCPU = 1;
					@(posedge clk);
					writeCPU = 0;
					c1out = `C1_NOP;
					farWay[set] = (farWay[set] === 1)? 0 : 1;
				end

			end else if (c1 === `C1_READ32) begin

				tag = a1[0:(`CACHE_TAG_SIZE - 1)];
				set = a1[`CACHE_TAG_SIZE:(`CACHE_SET_SIZE + `CACHE_TAG_SIZE - 1)];
				@(posedge clk);
				while (c1 !== `C1_READ32) @(posedge clk);
				offset = a1[0:(`CACHE_OFFSET_SIZE - 1)];
				if (flags[0][set][`VALID] === 1 && tags[0][set] === tag) begin
					if (c_dump === 1) begin
						$write("[Cache hit at 0 %b %b %b in READ32]\n", tag, set, offset);
					end
					for (i = 0; i < 5; i++) @(posedge clk); // memory response time simulation
					c1out = `C1_RESPONSE;
					d1out[0:7] = data[0][set][offset];
					d1out[8:15] = data[0][set][offset + 1];
					writeCPU = 1;
					@(posedge clk);
					d1out[0:7] = data[0][set][offset + 2];
					d1out[8:15] = data[0][set][offset + 3];
					@(posedge clk);
					writeCPU = 0;
					c1out = `C1_NOP;
					farWay[set] = 1;
				end else if (flags[1][set][`VALID] === 1 && tags[1][set] === tag) begin
					if (c_dump === 1) begin
						$write("[Cache hit at 1 %b %b %b in READ32]\n", tag, set, offset);
					end
					for (i = 0; i < 5; i++) @(posedge clk); // memory response time simulation
					c1out = `C1_RESPONSE;
					d1out[0:7] = data[1][set][offset];
					d1out[8:15] = data[1][set][offset + 1];
					writeCPU = 1;
					@(posedge clk);
					d1out[0:7] = data[1][set][offset + 2];
					d1out[8:15] = data[1][set][offset + 3];
					@(posedge clk);
					writeCPU = 0;
					c1out = `C1_NOP;
					farWay[set] = 0;
				end else begin
					if (c_dump === 1) begin
						$write("[Cache miss %b %b %b in READ32]\n", tag, set, offset);
					end
					for (i = 0; i < 3; i++) @(posedge clk); // memory response time simulation
					if (flags[farWay[set]][set][`DIRTY] === 1) begin
						sendData = 1;
						while (sendData === 1) @(posedge clk);
					end

					flags[farWay[set]][set][`VALID] = 0;
					a2out[0:(`CACHE_TAG_SIZE - 1)] = tag;
					a2out[`CACHE_TAG_SIZE:(`CACHE_SET_SIZE + `CACHE_TAG_SIZE - 1)] = set;
					c2out = `C2_READ_LINE;
					@(posedge clk);
					writeMEM = 0;
					c2out = `C2_NOP;
					waitData = 1;
					while(waitData === 1) @(posedge clk);
					writeMEM = 1;

					c1out = `C1_RESPONSE;
					d1out[0:7] = data[farWay[set]][set][offset];
					d1out[8:15] = data[farWay[set]][set][offset + 1];
					writeCPU = 1;
					@(posedge clk);
					d1out[0:7] = data[farWay[set]][set][offset + 2];
					d1out[8:15] = data[farWay[set]][set][offset + 3];
					@(posedge clk);
					writeCPU = 0;
					c1out = `C1_NOP;
					farWay[set] = (farWay[set] === 1)? 0 : 1;
				end

			end else if (c1 === `C1_INVALIDATE_LINE) begin

				tag = a1[0:(`CACHE_TAG_SIZE - 1)];
				set = a1[`CACHE_TAG_SIZE:(`CACHE_SET_SIZE + `CACHE_TAG_SIZE - 1)];
				@(posedge clk);
				if (flags[0][set][`VALID] === 1 && tags[0][set] === tag) begin
					if (flags[0][set][`DIRTY] === 1) begin
						sendData = 1;
						while (sendData === 1) @(posedge clk);
					end
					flags[0][set][`VALID] = 0;
					farWay[set] = 0;
				end else if (flags[1][set][`VALID] === 1 && tags[1][set] === tag) begin
					if (flags[1][set][`DIRTY] === 1) begin
						sendData = 1;
						while (sendData === 1) @(posedge clk);
					end
					flags[1][set][`VALID] = 0;
					farWay[set] = 1;
				end

			end else if (c1 === `C1_WRITE8) begin

				tag = a1[0:(`CACHE_TAG_SIZE - 1)];
				set = a1[`CACHE_TAG_SIZE:(`CACHE_SET_SIZE + `CACHE_TAG_SIZE - 1)];
				indata[0:7] = d1[0:7];
				@(posedge clk);
				while (c1 !== `C1_WRITE8) @(posedge clk);
				offset = a1[0:(`CACHE_OFFSET_SIZE - 1)];
				if (flags[0][set][`VALID] === 1 && tags[0][set] === tag) begin
					if (c_dump === 1) begin
						$write("[Cache hit at 0 %b %b %b in WRITE8]\n", tag, set, offset);
					end
					data[0][set][offset] = indata[0:7];
					flags[0][set][`DIRTY] = 1;
					for (i = 0; i < 5; i++) @(posedge clk); // memory response time simulation
				end else if (flags[1][set][`VALID] === 1 && tags[1][set] === tag) begin
					if (c_dump === 1) begin
						$write("[Cache hit at 1 %b %b %b in WRITE8]\n", tag, set, offset);
					end
					data[1][set][offset] = indata[0:7];
					flags[1][set][`DIRTY] = 1;
					for (i = 0; i < 5; i++) @(posedge clk); // memory response time simulation
				end else begin
					if (c_dump === 1) begin
						$write("[Cache miss %b %b %b in WRITE8]\n", tag, set, offset);
					end
					for (i = 0; i < 3; i++) @(posedge clk); // memory response time simulation
					if (flags[farWay[set]][set][`DIRTY] === 1) begin
						sendData = 1;
						while (sendData === 1) @(posedge clk);
					end

					flags[farWay[set]][set][`VALID] = 0;
					a2out[0:(`CACHE_TAG_SIZE - 1)] = tag;
					a2out[`CACHE_TAG_SIZE:(`CACHE_SET_SIZE + `CACHE_TAG_SIZE - 1)] = set;
					c2out = `C2_READ_LINE;
					@(posedge clk);
					writeMEM = 0;
					c2out = `C2_NOP;
					waitData = 1;
					while(waitData === 1) @(posedge clk);
					writeMEM = 1;

					data[farWay[set]][set][offset] = indata[0:7];
					flags[farWay[set]][set][`DIRTY] = 1;
					farWay[set] = (farWay[set] === 1)? 0 : 1;
				end

				c1out = `C1_RESPONSE;
				writeCPU = 1;
				@(posedge clk);
				writeCPU = 0;
				c1out = `C1_NOP;

			end else if (c1 === `C1_WRITE16) begin

				tag = a1[0:(`CACHE_TAG_SIZE - 1)];
				set = a1[`CACHE_TAG_SIZE:(`CACHE_SET_SIZE + `CACHE_TAG_SIZE - 1)];
				indata[0:15] = d1;
				@(posedge clk);
				while (c1 !== `C1_WRITE16) @(posedge clk);
				offset = a1[0:(`CACHE_OFFSET_SIZE - 1)];
				if (flags[0][set][`VALID] === 1 && tags[0][set] === tag) begin
					if (c_dump === 1) begin
						$write("[Cache hit at 0 %b %b %b in WRITE16]\n", tag, set, offset);
					end
					data[0][set][offset] = indata[0:7];
					data[0][set][offset + 1] = indata[8:15];
					flags[0][set][`DIRTY] = 1;
					for (i = 0; i < 5; i++) @(posedge clk); // memory response time simulation
				end else if (flags[1][set][`VALID] === 1 && tags[1][set] === tag) begin
					if (c_dump === 1) begin
						$write("[Cache hit at 1 %b %b %b in WRITE16]\n", tag, set, offset);
					end
					data[1][set][offset] = indata[0:7];
					data[1][set][offset + 1] = indata[8:15];
					flags[1][set][`DIRTY] = 1;
					for (i = 0; i < 5; i++) @(posedge clk); // memory response time simulation
				end else begin
					if (c_dump === 1) begin
						$write("[Cache miss %b %b %b in WRITE16]\n", tag, set, offset);
					end
					for (i = 0; i < 3; i++) @(posedge clk); // memory response time simulation
					if (flags[farWay[set]][set][`DIRTY] === 1) begin
						sendData = 1;
						while (sendData === 1) @(posedge clk);
					end

					flags[farWay[set]][set][`VALID] = 0;
					a2out[0:(`CACHE_TAG_SIZE - 1)] = tag;
					a2out[`CACHE_TAG_SIZE:(`CACHE_SET_SIZE + `CACHE_TAG_SIZE - 1)] = set;
					c2out = `C2_READ_LINE;
					@(posedge clk);
					writeMEM = 0;
					c2out = `C2_NOP;
					waitData = 1;
					while(waitData === 1) @(posedge clk);
					writeMEM = 1;

					data[farWay[set]][set][offset] = indata[0:7];
					data[farWay[set]][set][offset + 1] = indata[8:15];
					flags[farWay[set]][set][`DIRTY] = 1;
					farWay[set] = (farWay[set] === 1)? 0 : 1;
				end

				c1out = `C1_RESPONSE;
				writeCPU = 1;
				@(posedge clk);
				writeCPU = 0;
				c1out = `C1_NOP;

			end else if (c1 === `C1_WRITE32) begin

				tag = a1[0:(`CACHE_TAG_SIZE - 1)];
				set = a1[`CACHE_TAG_SIZE:(`CACHE_SET_SIZE + `CACHE_TAG_SIZE - 1)];
				indata[0:15] = d1;
				@(posedge clk);
				while (c1 !== `C1_WRITE32) @(posedge clk);
				offset = a1[0:(`CACHE_OFFSET_SIZE - 1)];
				indata[16:31] = d1;
				c1out = `C1_NOP;
				writeCPU = 1;
				if (flags[0][set][`VALID] === 1 && tags[0][set] === tag) begin
					if (c_dump === 1) begin
						$write("[Cache hit at 0 %b %b %b in WRITE32]\n", tag, set, offset);
					end
					data[0][set][offset] = indata[0:7];
					data[0][set][offset + 1] = indata[8:15];
					data[0][set][offset + 2] = indata[16:23];
					data[0][set][offset + 3] = indata[24:31];
					flags[0][set][`DIRTY] = 1;
					for (i = 0; i < 5; i++) @(posedge clk); // memory response time simulation
				end else if (flags[1][set][`VALID] === 1 && tags[1][set] === tag) begin
					if (c_dump === 1) begin
						$write("[Cache hit at 1 %b %b %b in WRITE32]\n", tag, set, offset);
					end
					data[1][set][offset] = indata[0:7];
					data[1][set][offset + 1] = indata[8:15];
					data[1][set][offset + 2] = indata[16:23];
					data[1][set][offset + 3] = indata[24:31];
					flags[1][set][`DIRTY] = 1;
					for (i = 0; i < 5; i++) @(posedge clk); // memory response time simulation
				end else begin
					if (c_dump === 1) begin
						$write("[Cache miss %b %b %b in WRITE32]\n", tag, set, offset);
					end
					for (i = 0; i < 3; i++) @(posedge clk); // memory response time simulation
					if (flags[farWay[set]][set][`DIRTY] === 1) begin
						sendData = 1;
						while (sendData === 1) @(posedge clk);
					end

					flags[farWay[set]][set][`VALID] = 0;
					a2out[0:(`CACHE_TAG_SIZE - 1)] = tag;
					a2out[`CACHE_TAG_SIZE:(`CACHE_SET_SIZE + `CACHE_TAG_SIZE - 1)] = set;
					c2out = `C2_READ_LINE;
					@(posedge clk);
					writeMEM = 0;
					c2out = `C2_NOP;
					waitData = 1;
					while(waitData === 1) @(posedge clk);
					writeMEM = 1;

					data[farWay[set]][set][offset] = indata[0:7];
					data[farWay[set]][set][offset + 1] = indata[8:15];
					data[farWay[set]][set][offset + 2] = indata[16:23];
					data[farWay[set]][set][offset + 3] = indata[24:31];
					flags[farWay[set]][set][`DIRTY] = 1;
					farWay[set] = (farWay[set] === 1)? 0 : 1;

				end

				c1out = `C1_RESPONSE;
				@(posedge clk);
				writeCPU = 0;
				c1out = `C1_NOP;

			end
		end
	end

endmodule

module memCTR #(parameter _SEED = 225526) (
	input clk, m_dump, reset,
	input[0:(`CACHE_TAG_SIZE + `CACHE_SET_SIZE - 1)] a2, inout[0:(`CACHE_DATA_SIZE - 1)] d2, inout[0:1] c2
	);

	reg [0:(`CACHE_DATA_SIZE - 1)] d2out;
	reg [0:1] c2out;
	reg write = 0;

	assign d2 = (write)? d2out : {`CACHE_DATA_SIZE{1'bz}};
	assign c2 = (write)? c2out : 2'bzz;

	integer SEED = _SEED;
	reg [7:0] data [0:(`CACHE_TAG_COUNT * `CACHE_SETS_COUNT)][0:`CACHE_LINE_COUNT];
	reg [0:(`CACHE_TAG_SIZE + `CACHE_SET_SIZE - 1)] addr;
	integer i = 0, j = 0;

	initial begin
		for (j = 0; j < `CACHE_TAG_COUNT * `CACHE_SETS_COUNT; j++) begin
			for (i = 0; i < `CACHE_LINE_COUNT; i += 1) begin
				data[j][i] = $random(SEED)>>16;
			end
		end
	end

	always @(posedge reset) begin
		write = 0;
		for (j = 0; j < `CACHE_TAG_COUNT * `CACHE_SETS_COUNT; j++) begin
			for (i = 0; i < `CACHE_LINE_COUNT; i += 1) begin
				data[j][i] = $random(SEED)>>16;
			end
		end
	end

	always @(posedge m_dump) begin
		$display("[Memory data]");
		for (j = 0; j < `CACHE_TAG_COUNT * `CACHE_SETS_COUNT; j++) begin
			$write("\t[%015b] ", j);
			for (i = 0; i < `CACHE_LINE_COUNT; i += 1) begin
				$write("%h", data[j][i]);
			end
			$write("\n");
		end
		$display("[Memory end data]");
	end

	always @(posedge clk) begin
		if (c2 === `C2_READ_LINE) begin


			for (i = 0; i < 100; i++) @(posedge clk); // memory response time simulation
			c2out = `C2_RESPONSE;
			addr = a2;
			write = 1;
			for (i = 0; i < `CACHE_LINE_COUNT && write; i += 2) begin
				d2out[0:7] = data[addr][i];
				d2out[8:15] = data[addr][i + 1];
				if (m_dump === 1) begin
					$display("[Memory send data] [%0d] %0d", i, data[addr][i]);
					$display("[Memory send data] [%0d] %0d", i+1, data[addr][i+1]);
				end
				@(posedge clk);
			end

			write = 0;
			c2out = `C2_NOP;

		end else if (c2 === `C2_WRITE_LINE) begin
			addr = a2;
			for (i = 0; i < `CACHE_LINE_COUNT && c2 === `C2_WRITE_LINE; i += 2) begin
				data[addr][i] = d2[0:7];
				data[addr][i + 1] = d2[8:15];
				if (m_dump === 1) begin
					$display("[Memory write data] [%0d] [%0d]", i, data[addr][i]);
					$display("[Memory write data] [%0d] [%0d]", i+1, data[addr][i+1]);
				end
				@(posedge clk);
			end
			for (i = 0; i < 100; i++) @(posedge clk); // menory response time simulation
			c2out = `C2_RESPONSE;
			write = 1;
			@(posedge clk);
			write = 0;
			c2out = `C2_NOP;

		end
	end
endmodule


`define M 64
`define N 60
`define K 32

`define A_PTR 0
`define B_PTR 2048 // = M * K
`define C_PTR 5888 // = M * K + 2 * N * K

module CPU (
	input clk,
	output[0:(`CACHE_TAG_SIZE + `CACHE_SET_SIZE - 1)] a1, inout[0:(`CACHE_DATA_SIZE - 1)] d1, inout[0:2] c1
	);

	reg [0:(`CACHE_TAG_SIZE + `CACHE_SET_SIZE - 1)] a1out;
	reg [0:(`CACHE_DATA_SIZE - 1)] d1out;
	reg [0:2] c1out = `C1_NOP;
	reg write = 1;

	assign a1 = (write)? a1out : {(`CACHE_TAG_SIZE + `CACHE_SET_SIZE - 1){1'bz}};
	assign d1 = (write)? d1out : {`CACHE_DATA_SIZE{1'bz}};
	assign c1 = (write)? c1out : 3'bzzz;

	reg [0:(`CACHE_ADDR_SIZE - 1)] pa, pb, pc, pt;
	reg [0:31] x;
	reg [0:31] y;
	reg [0:31] k;
	reg [0:31] s;
	reg [0:31] r1 = 0;
	reg [0:31] r2 = 0;
	integer i;

	initial begin
		@(posedge clk);

		pa = `A_PTR;
		@(posedge clk);

		pc = `C_PTR;
		@(posedge clk);


		@(posedge clk); // 1 tact for y = 0
		for (y = 0; y < `M; y++) begin
			@(posedge clk); // 1 tact loop

			@(posedge clk); // 1 tact for x = 0
			for (x = 0; x < `N; x++) begin
				@(posedge clk); // 1 tact loop

				pb = `B_PTR;
				@(posedge clk);

				s = 0;
				@(posedge clk);

				@(posedge clk); // 1 tact for k = 0
				for (k = 0; k < `K; k++) begin
					@(posedge clk);

					// Start pa[k]
					r1 = pa + k;
					a1out = r1[(32 - `CACHE_ADDR_SIZE):(31 - `CACHE_OFFSET_SIZE)]; // last bits
					c1out = `C1_READ8;
					@(posedge clk);
					a1out[0:(`CACHE_OFFSET_SIZE - 1)] = r1[(32 - `CACHE_OFFSET_SIZE):31];
					@(posedge clk);
					write = 0;
					c1out = `C1_NOP;
					while (c1 !== `C1_RESPONSE) @(posedge clk);
					r1[24:31] = d1[0:7];
					write = 1;
					// End pa[k]


					// Start pb[x]
					r2 = pb + 2 * x;
					a1out = r2[(32 - `CACHE_ADDR_SIZE):(31 - `CACHE_OFFSET_SIZE)]; // last bits
					c1out = `C1_READ16;
					@(posedge clk);
					a1out[0:(`CACHE_OFFSET_SIZE - 1)] = r2[(32 - `CACHE_OFFSET_SIZE):31];
					@(posedge clk);
					write = 0;
					c1out = `C1_NOP;
					while (c1 !== `C1_RESPONSE) @(posedge clk);
					r2[24:31] = d1[0:7];
					r2[16:23] = d1[8:15];
					write = 1;
					// End pb[x]

					s = s + r1 * r2;
					for (i = 0; i < 7; i++) @(posedge clk); // 1 tact: =; 1 tact: +; 5 tacts: *

					pb += 2 * `N;
					for (i = 0; i < 2; i++) @(posedge clk); // 1 tact: =; 1 tact: +

				end
				@(posedge clk); // exit loop

				// Start pc[x] = s
				r1 = pc + 4 * x;
				a1out = r1[(32 - `CACHE_ADDR_SIZE):(31 - `CACHE_OFFSET_SIZE)];
				d1out[0:7] = s[24:31];
				d1out[8:15] = s[16:23];
				c1out = `C1_WRITE32;
				@(posedge clk);
				a1out[0:(`CACHE_OFFSET_SIZE - 1)] = r1[(32 - `CACHE_OFFSET_SIZE):31];
				d1out[0:7] = s[8:15];
				d1out[8:15] = s[0:7];
				@(posedge clk);
				write = 0;
				@(posedge clk);
				while (c1 !== `C1_RESPONSE) @(posedge clk);
				c1out = `C1_NOP;
				write = 1;
				// End pc[x] = s

			end
			@(posedge clk); // exit loop

			pa += `K;
			for (i = 0; i < 2; i++) @(posedge clk); // 1 tact: =; 1 tact: +

			pc += 4 * `N;
			for (i = 0; i < 2; i++) @(posedge clk); // 1 tact: =; 1 tact: +

		end
		@(posedge clk); // exit loop

		$display("[End of programm at %d]", $time);
	end

endmodule

module run;
	reg clk = 0, c_dump = 0, m_dump = 0, reset = 0;
	wire [0:(`CACHE_TAG_SIZE + `CACHE_SET_SIZE - 1)] a1;
	wire [0:(`CACHE_DATA_SIZE - 1)] d1;
	wire [0:2] c1;
	wire [0:(`CACHE_TAG_SIZE + `CACHE_SET_SIZE - 1)] a2;
	wire [0:(`CACHE_DATA_SIZE - 1)] d2;
	wire [0:1] c2;

	CPU cpu(clk, a1, d1, c1);
	Cache cache(clk, c_dump, reset, a1, d1, c1, a2, d2, c2);
	memCTR mem(clk, m_dump, reset, a2, d2, c2);

	integer i;

	initial begin
		reset = 1;
		c_dump = 1;
		m_dump = 1;
		for (i = 0; i < 10000000; i++) begin
			clk = 1;
			#1;
			clk = 0;
		end
		#1;
		c_dump = 0;
		c_dump = 1;
		#1;
		m_dump = 0;
		m_dump = 1;
		#1;

		$finish;
	end
endmodule