import datetime
import os
import os.path
import shutil

import Image
import kid

class TestResult:
    most_recent = None
    baseline = None

def add_baseline(results):
    for f in os.listdir("baseline"):
        index, ext = os.path.splitext(f)

        if ext.lower() == ".png":
            try:
                index = int(index)
            except:
                continue
            else:
                results.setdefault(index, TestResult()).baseline = True

def add_most_recent(results):
    for f in os.listdir("recent"):
        name, ext = os.path.splitext(f)

        if ext.lower() == ".png":
            index, version = name.split("-")

            try:
                index = int(index)
                version = int(version)
            except:
                continue
            else:
                results[index].most_recent = max(
                    results.setdefault(index, TestResult()).most_recent,
                    version)

def description(index):
    baseline = file("baseline/%d.txt" % index).read()
    
class TestData:
    index = None

    failed = None

    header_class = None

    baseline_png = None
    baseline_description = None

    most_recent_png = None
    most_recent_description = None

def try_read(filename):
    try:
        return file(filename).read()
    except IOError:
        return None

def image_data(filename):
    try:
        return Image.open(filename).tostring()
    except IOError:
        return None

def tests():
    results = {}
    add_baseline(results)
    add_most_recent(results)

    tests = []
    for index, result in results.iteritems():
        t = TestData()
        t.index = index
        
        if result.baseline:
            t.baseline_png = "../baseline/%d.png" % index
            t.baseline_description = try_read("baseline/%d.txt" % index)
        if result.most_recent is not None:
            t.most_recent_png = "../recent/%d-%d.png" % \
                                (index, result.most_recent)
            t.most_recent_description = try_read("recent/%d-%d.txt" % \
                                                 (index, result.most_recent))

        if (t.baseline_description == t.most_recent_description) \
           and (image_data("baseline/%d.png" % index) == \
                image_data("recent/%d-%d.png" % \
                           (index, result.most_recent))):
            t.failed = False
            t.header_class = "success"
        else:
            t.failed = True
            t.header_class = "failure"

        tests.append(t)

    return tests

def main():
    data = tests()
    time = datetime.datetime.now()

    source_path = os.path.split(__file__)[0]
    template = os.path.join(source_path, "template", "gallery.html")
    css = os.path.join(source_path, "template", "gallery.css")

    all = kid.Template(template,
                       title="OVT Gallery: All Tests",
                       type="all",
                       time=time,
                       tests=data)
    all.write("gallery/all.html")

    failed = kid.Template(template,
                          title="OVT Gallery: Failed Tests",
                          type="failed",
                          time=time,
                          tests=[ t for t in data if t.failed ])
    failed.write("gallery/failed.html")
    
    shutil.copyfile(css, "gallery/gallery.css")

if __name__ == "__main__":
    main()
