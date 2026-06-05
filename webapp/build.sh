#!/bin/bash
# app.js / webapp_data.js 수정 후 이 스크립트를 실행하면
# index_src.html 에 둘을 인라인해 '자기완결 index.html' 을 다시 만든다.
cd "$(dirname "$0")"
python3 - <<'PY'
html=open('index_src.html',encoding='utf-8').read()
data=open('webapp_data.js',encoding='utf-8').read()
app =open('app.js',encoding='utf-8').read()
html=html.replace('<script src="webapp_data.js"></script>','<script>\n'+data+'\n</script>')
html=html.replace('<script src="app.js"></script>','<script>\n'+app+'\n</script>')
open('index.html','w',encoding='utf-8').write(html)
print('index.html 재생성 완료 (', round(len(html.encode())/1024), 'KB ) — 이 파일만 옮기면 됨')
PY
