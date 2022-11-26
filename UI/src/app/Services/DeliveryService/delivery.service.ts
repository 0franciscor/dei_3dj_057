import { Injectable } from '@angular/core';
import fetch from 'node-fetch';

@Injectable({
  providedIn: 'root'
})
export class DeliveryService {

  constructor() { }

  async getDeliveries(){
    const url = "http://localhost:3001/api/delivery/all";
    const response = await fetch(url, {
      method: 'GET',
      headers: {
        'Accept': 'application/json'
      }
    });
    const data = await response.json();
    return data;
  }

  async createDelivery(delivery: any) {
    const url = "http://localhost:3001/api/delivery/create";
    const data = delivery;
    const response = await fetch(url, {
      method: 'POST',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },
    })
    return response;
  }

  async updateDelivery(delivery: any) {
    const url = "http://localhost:3001/api/delivery/update";
    const data = delivery;
    const response = await fetch(url, {
      method: 'PATCH',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },
    })
    return response;
  }
}
