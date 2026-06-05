const A = APP_DATA;
const fmt = (x,d=3)=> (x>=0?'+':'') + x.toFixed(d);
const PDISP = {직구:'직구',싱커:'싱커',커터:'커터',슬라:'슬라이더',커브:'커브',체인지:'체인지업'};
const disp = p => PDISP[p] || p;   // 표시용 풀네임 (내부 키는 그대로)
const norm = s => s.normalize('NFD').replace(/[̀-ͯ]/g,'').toLowerCase();  // 악센트·대소문자 무시
let st = null, curPos = '타자', curSearch = '';

function buildOptions(){           // 포지션·검색에 맞는 선수만 드롭다운에 채움
  const sel=document.getElementById('player'); sel.innerHTML='';
  const q=norm(curSearch);
  const list=A.players.map((p,i)=>({p,i}))
    .filter(o=> o.p.side===curPos && (!q || norm(o.p.name).includes(q)));
  list.forEach(o=>{ const el=document.createElement('option'); el.value=o.i;
    el.textContent=`${o.p.name} (${o.p.opp}${curPos==='타자'?'PA':'BF'})`; sel.appendChild(el); });
  document.getElementById('pcount').textContent = `${list.length}명`;
  if(list.length){ sel.value=list[0].i; selectPlayer(list[0].i); }
}

function init(){
  if(window.__appInit) return;            // 중복 실행 방지 (Canva 등 임베드에서 2번 실행돼 버튼 중복되는 것 차단)
  window.__appInit = true;
  const sel = document.getElementById('player');
  document.querySelectorAll('.posbtn').forEach(b=> b.onclick=()=>{
    curPos=b.dataset.pos;
    document.querySelectorAll('.posbtn').forEach(x=>x.classList.toggle('sel',x===b));
    buildOptions(); });
  document.getElementById('search').oninput = e=>{ curSearch=e.target.value; buildOptions(); };
  // 구종 버튼
  const pr=document.getElementById('pitchrow'); pr.innerHTML='';   // 재실행돼도 중복 안 되게 비우고 채움
  A.pitches.forEach(pg=>{
    const b=document.createElement('button'); b.className='btn'; b.textContent=disp(pg); b.dataset.pg=pg;
    b.onclick=()=>{ st.selPg=pg; document.querySelectorAll('#pitchrow .btn').forEach(x=>x.classList.remove('sel')); b.classList.add('sel'); };
    pr.appendChild(b);
  });
  // 존 클릭
  document.querySelectorAll('.zone').forEach(z=>{
    z.onclick=()=>{ st.selZone=+z.dataset.zone;
      document.querySelectorAll('.zone').forEach(x=>x.classList.remove('sel')); z.classList.add('sel'); };
  });
  // 결과 버튼
  document.querySelectorAll('#outrow .btn').forEach(b=> b.onclick=()=>onOutcome(b.dataset.out));
  // 타구질(EV·LA) 입력: 프리셋 + 슬라이더 + 확정
  const PRESETS=[{n:'약한 땅볼',ev:78,la:-8},{n:'평범한 뜬공',ev:88,la:38},
                 {n:'빗맞은 타구',ev:82,la:14},{n:'강한 라인드라이브',ev:101,la:14},{n:'완벽한 배럴',ev:106,la:26}];
  const ps=document.getElementById('presets'); ps.innerHTML='';
  PRESETS.forEach(p=>{ const b=document.createElement('button'); b.className='preset';
    b.textContent=`${p.n} (EV${p.ev}·LA${p.la})`;
    b.onclick=()=> commitHit(p.ev, p.la);   // 프리셋 = 원클릭 즉시 타석 종료
    ps.appendChild(b); });
  document.getElementById('evSlider').oninput=updatePreview;
  document.getElementById('laSlider').oninput=updatePreview;
  document.getElementById('hitConfirm').onclick=()=>{ const [ev,la]=curEVLA(); commitHit(ev,la); };
  sel.onchange=()=>selectPlayer(+sel.value);
  buildOptions();          // 초기: 타자 목록 + 첫 선수 선택
}

function baseMetrics(p,c){
  const dpp=p.DRV_total/p.opp, w=p.opp/(p.opp+c.Kplayer);
  const rs=c.leag+(dpp-c.leag)*w, PLUS=100+(rs-c.leag)*c.scale;
  const WAR=((p.DRV_total/p.N_p)-c.league_pp+A.r)*p.N_p/A.RPW;
  return {DRV:p.DRV_total, PLUS, WAR};
}
function selectPlayer(idx){
  const p=A.players[idx], c=A.league[p.side];
  st={ base:p, idx, consts:c, sgn:(p.side==='투수'?-1:1), bm:baseMetrics(p,c),
       selZone:null, selPg:null, balls:0, strikes:0, paProcess:0,
       sessionDelta:0, pitches:0, donePAs:0, log:[] };
  const unit = p.side==='타자' ? 'PA' : 'BF';
  document.getElementById('pbase').innerHTML =
    `<b style="color:${p.side==='타자'?'var(--blue)':'var(--red)'}">${p.side}</b> · `+
    `기준값 ${p.opp} ${unit} · ${p.N_p} 투구 · DRV ${p.DRV_total.toFixed(1)}런 · `+
    `DRV+ ${st.bm.PLUS.toFixed(0)} · DRWAR ${st.bm.WAR.toFixed(2)}승`;
  recompute(); render(); clearSel();
}
function clearSel(){ st.selZone=null; st.selPg=null;
  document.querySelectorAll('.zone,.btn[data-pg]').forEach(x=>x.classList.remove('sel'));
  hideHitPanel(); }

const eh=(c,pg,z)=> (A.E[`${c}|${pg}|${z}`] ?? A.muE);
const bh=(c,pg,z)=> (A.B[`${c}|${pg}|${z}`] ?? A.muB);
const av=(ev,la)=>{ const eb=Math.floor(ev/5)*5, lb=Math.floor(la/5)*5; return A.A[`${eb}_${lb}`] ?? A.muA; }; // 타구질 A[EV,LA]
const cstr=(b,s)=>`${b}B-${s}S`;

// ── 타구질(EV·LA) 입력 패널 ────────────────────────────────────────────────
function curEVLA(){ return [ +document.getElementById('evSlider').value, +document.getElementById('laSlider').value ]; }
function updatePreview(){ const [ev,la]=curEVLA();
  document.getElementById('evVal').textContent=ev; document.getElementById('laVal').textContent=la;
  document.getElementById('avPreview').innerHTML =
    `이 타구질의 가치 A = <b>${fmt(av(ev,la))}</b> <small>(단타·2루타 구분 없이 EV·LA로만 결정)</small>`; }
function showHitPanel(){ document.getElementById('hitpanel').style.display='block'; updatePreview(); }
function hideHitPanel(){ document.getElementById('hitpanel').style.display='none'; }
function commitHit(ev,la){
  if(st.selZone==null||st.selPg==null) return flash('존·구종을 먼저 선택하세요');
  const z=st.selZone, pg=st.selPg, cb=cstr(st.balls,st.strikes);
  const V=av(ev,la), B=bh(cb,pg,z), ps=V-B, ctrP=st.sgn*ps;
  st.sessionDelta+=ctrP; st.pitches+=1;
  st.log.push({n:st.pitches, out:`타격 EV${ev}·LA${la}`, z, pg, terminal:true, contrib:ctrP, count:'종료'});
  st.donePAs+=1; st.balls=0; st.strikes=0; st.paProcess=0;
  document.getElementById('laststep').innerHTML =
    `<b>타석종료 · 인플레이 타격</b> · EV ${ev}·LA ${la} → 타구질 V <b>${fmt(V)}</b> − 허들 ${fmt(B)} = 이 타석 가치 <b>${fmt(ctrP)}</b>${st.sgn<0?' <small>(투수 기준)</small>':''}`;
  hideHitPanel(); clearSel(); recompute(); render();
}

function onOutcome(out){
  if(st.selZone==null) return flash('존을 먼저 클릭하세요');
  if(st.selPg==null)   return flash('구종을 먼저 선택하세요');
  const z=st.selZone, pg=st.selPg, inZone=(z>=1&&z<=9);
  if(out==='데드볼' && ![1,4,7,11,13].includes(z))
    return flash('데드볼은 몸쪽(1·4·7·11·13)에서만 가능합니다');
  if(out==='타격'){ showHitPanel(); return; }   // 인플레이 타구 → 타구질(EV·LA) 입력 패널
  const cb=cstr(st.balls,st.strikes);
  let terminal=false, kind=null, isStrike=false, isBall=false;
  if(out==='헛스윙') isStrike=true;
  else if(out==='루킹'){ inZone?isStrike=true:isBall=true; }
  else if(out==='파울'){ if(st.strikes<2) isStrike=true; /* 2스트라이크 파울=유지 */ }
  else if(out==='데드볼'){ terminal=true; kind='HBP'; }

  let ctr=0, desc='', isProcess=false;   // ctr = 타자 관점 가치
  const persp = st.sgn<0 ? ' <small>(투수 막은 가치)</small>' : '';
  if(!terminal){
    let nb=st.balls, ns=st.strikes;
    if(isStrike){ ns++; if(ns===3){terminal=true; kind='K';} }
    else if(isBall){ nb++; if(nb===4){terminal=true; kind='BB';} }
    if(!terminal){
      const ca=cstr(nb,ns);
      const dRE=(A.RE[ca]??0)-(A.RE[cb]??0), E=eh(cb,pg,z);
      ctr=dRE-E; isProcess=true; st.balls=nb; st.strikes=ns;
      desc=`<b>중간투구</b> · ${out} (${cb}→${ca}) · 이 공 가치 <b>${fmt(st.sgn*ctr)}</b>${persp}`;
    }else{
      const V=(kind==='K')?A.vK:A.vBB; ctr=V-bh(cb,pg,z);
      desc=`<b>타석종료 · ${kind==='K'?'삼진':'볼넷'}</b> · 이 타석 가치 <b>${fmt(st.sgn*ctr)}</b>${persp}`;
    }
  }
  if(terminal && !isProcess && ctr===0){
    const V=(kind==='HBP')?A.vHBP:(kind==='HR')?A.VHR:A.Vsingle; ctr=V-bh(cb,pg,z);
    const knm={HBP:'데드볼',HIT:'안타',HR:'홈런'}[kind];
    desc=`<b>타석종료 · ${knm}</b> · 이 타석 가치 <b>${fmt(st.sgn*ctr)}</b>${persp}`;
  }

  const ctrP = st.sgn * ctr;             // 선수(타자/투수) 관점 가치
  if(isProcess) st.paProcess += ctrP;
  st.sessionDelta += ctrP;
  st.pitches += 1;
  st.log.push({n:st.pitches, out, z, pg, terminal, contrib:ctrP,
               count: terminal?'종료':cstr(st.balls,st.strikes)});
  if(terminal){ st.donePAs+=1; st.balls=0; st.strikes=0; st.paProcess=0; }
  document.getElementById('laststep').innerHTML = desc;
  clearSel(); recompute(); render();
}

function recompute(){
  const b=st.base, c=st.consts;
  const DRV=b.DRV_total+st.sessionDelta, Np=b.N_p+st.pitches, OPP=b.opp+st.donePAs;
  const dpp=DRV/OPP, w=OPP/(OPP+c.Kplayer), rs=c.leag+(dpp-c.leag)*w;
  st.cur={ DRV, Np, OPP, PLUS:100+(rs-c.leag)*c.scale, WAR:((DRV/Np)-c.league_pp+A.r)*Np/A.RPW };
}

function setMetric(idv,idd,cur,base,dec,wins){
  const dl=cur-base, dir=Math.abs(dl)<1e-9?'flat':(dl>0?'up':'down');
  const v=document.getElementById(idv);
  v.textContent=cur.toFixed(dec); v.className='val '+dir;     // 값 숫자도 상승=초록/하락=빨강
  const el=document.getElementById(idd);
  el.textContent = dir==='flat' ? '기준과 동일'
                 : `${dl>=0?'▲':'▼'} ${(dl>=0?'+':'')+dl.toFixed(dec)}${wins||''} (기준 ${base.toFixed(dec)})`;
  el.className='dlt '+dir;
}
function render(){
  document.getElementById('count').textContent=`${st.balls} - ${st.strikes}`;
  setMetric('mDRV','dDRV', st.cur.DRV, st.bm.DRV, 1);
  setMetric('mPLUS','dPLUS', st.cur.PLUS, st.bm.PLUS, 0);
  setMetric('mWAR','dWAR', st.cur.WAR, st.bm.WAR, 2);
  document.getElementById('pastat').textContent =
    `누적 ${st.pitches} 투구 · 완료 타석 ${st.donePAs} · 이번 타석 과정합 ${fmt(st.paProcess)} · 세션 가치 ${fmt(st.sessionDelta)}런`;
  const log=document.getElementById('log');
  log.innerHTML = st.log.slice().reverse().map(r=>{
    const zl = (r.z<=9?'존'+r.z:'외곽'+r.z);
    return `<div class="row ${r.terminal?'term':''}">
      <span>${r.n}구 · ${disp(r.pg)} · ${zl} · ${r.out} → ${r.count}</span>
      <span class="${r.contrib>=0?'pos':'neg'}">${fmt(r.contrib)}</span></div>`;
  }).join('');
}
function resetSession(){ selectPlayer(st.idx); flash('세션 초기화'); }

let ft;
function flash(msg){ const f=document.getElementById('flash'); f.textContent=msg;
  f.classList.add('on'); clearTimeout(ft); ft=setTimeout(()=>f.classList.remove('on'),1400); }

init();
