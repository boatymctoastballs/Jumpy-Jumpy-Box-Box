onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /lab_tb/clk
add wave -noupdate /lab_tb/rst
add wave -noupdate /lab_tb/ca
add wave -noupdate /lab_tb/cb
add wave -noupdate /lab_tb/cc
add wave -noupdate /lab_tb/cd
add wave -noupdate /lab_tb/ce
add wave -noupdate /lab_tb/cf
add wave -noupdate /lab_tb/cg
add wave -noupdate /lab_tb/dp
add wave -noupdate /lab_tb/Hsync
add wave -noupdate /lab_tb/Vsync
add wave -noupdate /lab_tb/an
add wave -noupdate /lab_tb/tb_running
add wave -noupdate /lab_tb/vgaRed
add wave -noupdate /lab_tb/vgaGreen
add wave -noupdate /lab_tb/vgaBlue
add wave -noupdate /lab_tb/uut/clk
add wave -noupdate /lab_tb/uut/rst
add wave -noupdate /lab_tb/uut/vgaRed
add wave -noupdate /lab_tb/uut/vgaGreen
add wave -noupdate /lab_tb/uut/vgaBlue
add wave -noupdate /lab_tb/uut/ca
add wave -noupdate /lab_tb/uut/cb
add wave -noupdate /lab_tb/uut/cc
add wave -noupdate /lab_tb/uut/cd
add wave -noupdate /lab_tb/uut/ce
add wave -noupdate /lab_tb/uut/cf
add wave -noupdate /lab_tb/uut/cg
add wave -noupdate /lab_tb/uut/dp
add wave -noupdate /lab_tb/uut/Hsync
add wave -noupdate /lab_tb/uut/Vsync
add wave -noupdate /lab_tb/uut/an
add wave -noupdate -radix unsigned /lab_tb/uut/xctr
add wave -noupdate -radix unsigned /lab_tb/uut/yctr
add wave -noupdate /lab_tb/uut/pixel
add wave -noupdate /lab_tb/uut/a
add wave -noupdate /lab_tb/uut/b
add wave -noupdate /lab_tb/uut/c
add wave -noupdate /lab_tb/uut/d
add wave -noupdate /lab_tb/uut/a0
add wave -noupdate /lab_tb/uut/a1
add wave -noupdate /lab_tb/uut/a2
add wave -noupdate /lab_tb/uut/b0
add wave -noupdate /lab_tb/uut/b1
add wave -noupdate /lab_tb/uut/b2
add wave -noupdate /lab_tb/uut/c0
add wave -noupdate /lab_tb/uut/c1
add wave -noupdate /lab_tb/uut/c2
add wave -noupdate /lab_tb/uut/nr
add wave -noupdate /lab_tb/uut/ctr
add wave -noupdate /lab_tb/uut/hs
add wave -noupdate /lab_tb/uut/vs
add wave -noupdate /lab_tb/uut/bildminne
add wave -noupdate /lab_tb/uut/video
add wave -noupdate /lab_tb/uut/xpix
add wave -noupdate /lab_tb/uut/ypix
add wave -noupdate -radix unsigned /lab_tb/uut/kol
add wave -noupdate -radix unsigned /lab_tb/uut/rad
add wave -noupdate /lab_tb/uut/led/clk
add wave -noupdate /lab_tb/uut/led/rst
add wave -noupdate /lab_tb/uut/led/ca
add wave -noupdate /lab_tb/uut/led/cb
add wave -noupdate /lab_tb/uut/led/cc
add wave -noupdate /lab_tb/uut/led/cd
add wave -noupdate /lab_tb/uut/led/ce
add wave -noupdate /lab_tb/uut/led/cf
add wave -noupdate /lab_tb/uut/led/cg
add wave -noupdate /lab_tb/uut/led/dp
add wave -noupdate /lab_tb/uut/led/an
add wave -noupdate /lab_tb/uut/led/ledvalue
add wave -noupdate /lab_tb/uut/led/segments
add wave -noupdate /lab_tb/uut/led/counter_r
add wave -noupdate /lab_tb/uut/led/v
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {0 ns} 0}
configure wave -namecolwidth 150
configure wave -valuecolwidth 42
configure wave -justifyvalue left
configure wave -signalnamewidth 1
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits us
update
WaveRestoreZoom {973077 ns} {1001417 ns}
