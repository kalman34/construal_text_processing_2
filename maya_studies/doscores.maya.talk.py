from __future__ import division
import csv
import re

## Builds concreteness dictionary
conc_dict = {}
with open("concreteness.csv", "r") as f:  # change name of file and put in here
    reader = csv.reader(f)
    next(reader)  # .next skips header
    for row in reader:
        conc_dict[row[0]] = float(row[2])

with open("14_forkal_talk_181210.csv", "r", encoding="latin1") as f:
    with open("scores_talk_test.csv", "w") as g:
        reader = csv.reader(f, quotechar='"')
        writer = csv.writer(g, lineterminator = '\n')
        writer.writerow(["id", "msg_sum", "msg_avg", "rec_sum", "rec_avg", "msg_word_count", "rec_word_count"])
        reader.__next__()
        for row in reader:
            id_ = row[0]
            print(id_)
            message = row[1].split()
            recall = row[2].split()
            to_write = [id_]
            # If a value is missing in a cell, skip this ro
            if message == [] or recall == []:
                writer.writerow(["Message or Recall is empty"])
                continue

            msg_sum = 0
            msg_word_count = 0
            length = len(message)

            for i in range(0, length):
                word1 = re.sub("[\.,-/\\?!;: ]", "", message[i]).lower()

                # If i+1 is a valid index...
                if i + 1 <= length - 1:
                    word2 = re.sub("[\.,-/\\?!;: ]", "", message[i + 1]).lower()
                    phrase = word1 + " " + word2
                else:
                    phrase = word1


                if phrase in conc_dict:
                    msg_sum += conc_dict[phrase]
                    msg_word_count += 1
                else:
                    if i == 0 and word1 in conc_dict:
                        msg_sum += conc_dict[word1]
                        msg_word_count += 1
                    if word2 in conc_dict:
                        msg_sum += conc_dict[word2]
                        msg_word_count += 1

            to_write.append(str(msg_sum))
            if msg_word_count != 0:
                to_write.append(str(msg_sum/float(msg_word_count)))
            else:
                to_write.append("")

            rec_sum = 0
            rec_word_count = 0
            for i in range(0, len(recall) - 1):
                word1 = re.sub("[\.,-/\\?!;: ]", "", recall[i]).lower()
                word2 = re.sub("[\.,-/\\?!;: ]", "", recall[i + 1]).lower()
                phrase = word1 + " " + word2
                if phrase in conc_dict:
                    rec_sum += conc_dict[phrase]
                    rec_word_count += 1
                else:
                    if i == 0 and word1 in conc_dict:
                        rec_sum += conc_dict[word1]
                        rec_word_count += 1
                    if word2 in conc_dict:
                        rec_sum += conc_dict[word2]
                        rec_word_count += 1
            to_write.append(str(rec_sum))
            if rec_word_count != 0:
                to_write.append(str(rec_sum/float(rec_word_count)))
            else:
                to_write.append("")
            to_write.append(str(msg_word_count))
            to_write.append(str(rec_word_count))
            writer.writerow(to_write)
