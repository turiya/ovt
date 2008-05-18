import shutil
import sys

import gallery

def copy_baselines(baselines):
    for index, result in baselines.iteritems():
        recent_image = "recent/%d-%d.png" % (index, result.most_recent)
        baseline_image = "baseline/%d.png" % index
        recent_description = "recent/%d-%d.txt" % (index, result.most_recent)
        baseline_description = "baseline/%d.txt" % index
        print "copied %s -> %s" % (recent_image, baseline_image)
        shutil.copyfile(recent_image, baseline_image)
        print "copied %s -> %s" % (recent_description, baseline_description)
        shutil.copyfile(recent_description, baseline_description)

def main():
    if len(sys.argv) < 2:
        print "nothing to do"
        return 0

    results = {}
    gallery.add_most_recent(results)

    if sys.argv[1] == "all":
        print "baselining all"
        copy_baselines(results)
    else:
        print "baselining some"
        some_results = {}
        for index in [ int(a) for a in sys.argv[1:] ]:
            if results.has_key(index):
                some_results[index] = results[index]
            else:
                print "index %d not found" % index

        copy_baselines(some_results)

    gallery.main()
    print "updated gallery"

if __name__ == "__main__":
    main()
