Using BypassFifo to connect stages looks good for IPC.
But that also made all connected rules fire in one cycle because of a long combinational circuit.

SixStage Bypass

                Cycles 			Insts			IPC
                
median			6049			4243			0.

multiply		22170			20893			0.

qsort			162696			123496			0.

tower			4602			4168			0.

vvad			2711			2408			0.


SixStageBHT

                Cycles 			Insts			IPC
                
median			5193			4243			0.

multiply		21693			29893			0.

qsort			146795			123496			0.

tower			4485			4168			0.

vvad			2709			2408			0.