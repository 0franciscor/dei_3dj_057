import { Injectable } from '@angular/core';
import fetch from 'node-fetch';

@Injectable({
  providedIn: 'root'
})
export class TruckService {

  constructor() { }

  async getTruck() {
    const url = 'http://localhost:3000/api/truck/all';
    let test: any[] = [];
    await fetch(url, {
      method: 'GET',
      headers: {
        'Accept': 'application/json'
      }
    }).then(res => res.json().then(data => {test=data;}));

    return test;
  }

  async createTruck(truck: any) {
    const url = 'http://localhost:3000/api/truck/';
    const data = truck;
    // const httpAgent = new http.Agent({ rejectUnauthorized: false });
    fetch(url, {
      method: 'POST',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },
      // agent: httpAgent
    })
  }
}
