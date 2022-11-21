import { Injectable } from '@angular/core';
import fetch from 'node-fetch';

@Injectable({
  providedIn: 'root'
})
export class DeliveryService {

  constructor() { }

  async getDelivery(){
    const url = "https://localhost:5001/api/deliveries/getAll";
    let receivedDelivery: any[] = [];
    await fetch(url, {
      method: 'GET',
      headers: {
        'Accept': 'application/json'
      }
    }).then(res => res.json().then(data => {receivedDelivery = data;}));

    return receivedDelivery;
  }

  async createDelivery(delivery: any) {
    const url = "https://localhost:5001/api/deliveries/getAll";
    const data = delivery;
    fetch(url, {
      method: 'POST',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },
    })
  }
}
