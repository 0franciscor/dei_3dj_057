import { Injectable } from '@angular/core';
import fetch from 'node-fetch';
import * as http from "http";

@Injectable({
  providedIn: 'root'
})
export class WarehouseService {

  constructor() { }

  async createWarehouse(warehouse: any) {
    const url = 'http://localhost:3001/api/warehouse/create';
    const data = warehouse;
    
    let answer;

    const response = fetch(url, {
      method: 'POST',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },

    }).then(res => res.json().then(data => {answer=data;}));

    
  }

  async getAllWarehouses() {
    let test: any[] = [];
    const url = 'http://localhost:3001/api/warehouse/all';
    const response = await fetch(url, {
      method: 'GET'
    }).then(res => res.json().then(data => {test=data;}));
    console.log(test);
    return test;
    
  }



}
