import { Injectable } from '@angular/core';
import fetch from 'node-fetch';

@Injectable({
  providedIn: 'root'
})
export class TruckService {

  constructor() { }

  async getTruck() {
    const url = 'http://localhost:3000/api/truck/all';
    const response = await fetch(url, {
      method: 'GET',
      headers: {
        'Accept': 'application/json'
      }
    });

    const data = await response.json();

    return data;
  }

  async createTruck(truck: any) {
    const url = 'http://localhost:3000/api/truck/';
    const data = truck;
    const response = await fetch(url, {
      method: 'POST',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },
      // agent: httpAgent
    })
    console.log(response);

  }
}
