import { Identifier } from './Identifier'
const { 
  v1: uuidv1,
  v4: uuidv4,
} = require('uuid');

export class UniqueEntityID extends Identifier<string | number>{
  constructor (id?: string | number) {
    super(id ? id : uuidv4());
  }
}