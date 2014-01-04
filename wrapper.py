
import argparse
import multiprocessing
import subprocess
import sys

moperf = "./moperf.native"
mosynth = "./mosynth.native"

def run_moperf(modes):
    fname = '/tmp/modes'
    # write modes to a tmp file
    with open(fname, mode='w') as f:
        for mode in modes:
            f.write(mode + "\n")
    # run moperf
    cmd = [moperf, fname]
    return subprocess.check_output(cmd).decode("utf-8")

def run_mosynth(depth, prepend=None, init=None, ops=None):
    cmd = [mosynth, "-block-depth", str(depth)]
    if prepend:
        cmd += ['-prepend', prepend]
    if init:
        cmd += ['-init', init]
    if ops:
        cmd += ['-ops', ops]
    out = subprocess.check_output(cmd).decode("utf-8")
    modes = []
    possible, potential, found = 0, 0, 0
    for line in out.split("\n"):
        if line.startswith(":"):
            n = line.rsplit(" ", 1)[-1]
            if "possible" in line:
                possible = n
            elif "potential" in line:
                potential = n
            elif "found" in line:
                found = n
        elif line.startswith("["):
            modes.append(line)
    return modes, possible, potential, found

def map_f(value):
    print(value, file=sys.stderr)
    depth, prepend, init, ops = value
    return run_mosynth(depth, prepend=prepend, init=init, ops=ops)

def callback(results):
    modelist = []
    possible, potential, found = 0, 0, 0
    for result in results:
        modes, pos, pot, fnd = result
        modelist.extend(modes)
        possible += int(pos)
        potential += int(pot)
        found += int(fnd)
    schemes = run_moperf(modelist)
    print("Found Schemes:")
    print(schemes)
    print("Statistics:")
    print(": possible modes:", possible)
    print(": potential modes:", potential)
    print(": found modes (w/ dups):", found)
    print(": found modes (unique):", schemes.count('\n'))

def main(argv):
    parser = argparse.ArgumentParser(
        description='Parallelization wrapper for motools.')
    parser.add_argument('depth', type=int,
                        help='block depth to search')
    parser.add_argument('nprocs', type=int,
                        help='number of processes to use')
    parser.add_argument('--init', type=str, default=None,
                        help='init string to use')
    parser.add_argument('--ops', type=str, default="",
                        help='value to pass to -ops flag of synthesizer')
    args = parser.parse_args()

    insts = ["DUP", "GENRAND", "GENZERO", "INC", "M", "NEXTIV", "OUT", "PRP", "XOR",
             "SWAP", "2SWAP"]

    ops = args.ops.split(",")
    for op in ops:
        if op.startswith("+"):
            if op[1:] not in insts:
                insts.append(op[1:])
        if op.startswith("-"):
            insts.remove(op[1:])

    # print(insts)

    dsplit = 7
    pool = multiprocessing.Pool(processes=args.nprocs)
    if args.depth >= dsplit:
        # above a certain depth we start using the prepending option
        l = [(i, None, args.init, args.ops) for i in range(1, dsplit)]
        for depth in range(dsplit, args.depth + 1):
            l.extend([(depth, i, args.init, args.ops) for i in insts])
    else:
        l = [(i, None, args.init) for i in range(1, args.depth + 1)]
    pool.map_async(map_f, l, callback=callback)
    pool.close()
    pool.join()

if __name__ == '__main__':
    try:
        main(sys.argv)
    except KeyboardInterrupt:
        pass
