#!/usr/bin/python3


# Read lines from input file

infile = open("ToaqDictionary.csv", "r")
inlines = infile.readlines()
infile.close()

# Open output file, skip lines that are already done

outfile_name = input("\nEnter name for output file > ")
outfile = open(outfile_name, "a")
outfile_read = open(outfile_name, "r")

for outline in outfile_read:
    if outline.split(",")[0] == inlines[0].split(",")[0]:
        inlines = inlines[1:]

outfile_read.close()


valid_frames = ["POQ", "PAI", "FA",
                "DUA", "LEO", "CHEO",
                "DUASUE", "KUOI", "TIAO",
                "PU", "JIPA",
                "JIE", "FUI",
                "CA", "JEO", "CUA", "SOQ", "KOE",
                "?",  # Used when I don't know the type
                "-",] # Used for non-predicates

poq_shorts = ["P", "A", "C"] # Shorthands for POQ
quit_shorts = ["Q", "QUIT"]

valid_frames += poq_shorts
valid_frames += quit_shorts


def getFrame():
    frame = input("Which frame? > ")
    frame = frame.upper()
    
    while not (frame in valid_frames):
        frame = input("Invalid frame. Try again? > ")
        frame = frame.upper()
    
    if frame in poq_shorts:
        frame = "POQ"

    return frame
        

for line in inlines:
    csv = line.strip().split(",")
    print("\n\n" + csv[0])
    print(csv[3] + "\n")
    frame = getFrame()

    if frame in quit_shorts:
        break
    
    outfile.write(csv[0] + ","
                  + frame + "\n")
    outfile.flush()

outfile.close()