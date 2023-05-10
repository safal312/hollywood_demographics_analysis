import subprocess
import time

def scurl(URL, delay=3):
    proc = subprocess.Popen(["curl", URL], stdout=subprocess.PIPE)

    time.sleep(delay)
    (out, err) = proc.communicate()
    
    # print(out)
    if len(out) == 0 or err != None:
        print("There has been an error:", err)
        # return None
    else:
        return out

# scurl('https://thescriptlab.com/?post_type=property&p=35626')