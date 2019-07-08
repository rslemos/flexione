import * as pl from 'tau-prolog';
import flex from './flex.pro';

let session = pl.create(1000);
console.log('prolog', flex);

session.consult(flex);
//session.query('horizontal(line(point(2,3),P)).');
//session.query('crosswd(V1,V2,V3,H1,H2,H3).');
//session.query('add(succ(succ(succ(0))),succ(succ(0)),R)');
session.query('add(0,1,R)');
session.answers(answer => console.log(pl.format_answer(answer)));