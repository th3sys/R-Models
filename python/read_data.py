import json
import os

inx = 10000.0
for root, dirs, files in os.walk('../IG/json'):
    for file in files:
        if file.endswith('json'):
            fn = '%s%s%s' % (root, os.sep, file)
            csv = '%s%s%s' % (root.replace('json', 'csv'), os.sep, file.replace('json', 'csv'))
            print(fn)
            o = open(csv, 'w')
            o.write('Time,Open,High,Low,Close,Volume \n')
            with open(fn, 'r') as f:
                text = json.loads(f.read())
                prices = text['prices']
                for price in prices:
                    t = ''
                    try:
                        t = price['snapshotTime'].replace('/', '.').replace('T', ' ')
                        opn = (price['openPrice']['ask'] + price['openPrice']['bid']) / (2.0*inx)
                        high = (price['highPrice']['ask'] + price['highPrice']['bid']) / (2.0*inx)
                        low = (price['lowPrice']['ask'] + price['lowPrice']['bid']) / (2.0*inx)
                        cls = (price['closePrice']['ask'] + price['closePrice']['bid']) / (2.0*inx)
                        o.write('%s,%s,%s,%s,%s,1\n' % (t, opn, high, low, cls))
                    except Exception as ex:
                        print('%s, %s' % (t, ex))
            o.close()

