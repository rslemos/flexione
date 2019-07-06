import * as pl from 'tau-prolog';
import flex from './flex.pro';

let session = pl.create(1000);
console.log('prolog', flex);

session.consult(flex);
session.query('likes(Y, X), likes(Y, Z).');
session.answers(answer => console.log(answer));