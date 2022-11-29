import { Injectable } from '@angular/core';
import fetch from 'node-fetch';
import * as http from "http";


@Injectable({
  providedIn: 'root'
})
export class WarehouseService {

  constructor() { }

  async getWarehouse(WarehouseId:string) {
    console.log(WarehouseId);
    const url = 'http://localhost:3001/api/warehouse/'+ WarehouseId;
    const urlBackUp = "http://5.249.66.111:3001/api/warehouse/"+WarehouseId;
    try {
      
    const response = await fetch(url, {
      method: 'GET',
      headers: {
        'Accept': 'application/json'
      }
    });

    const data = await response.json();

    return data;
    } catch {
        
      const response = await fetch(urlBackUp, {
        method: 'GET',
        headers: {
          'Accept': 'application/json'
        }
      });

      const data = await response.json();

      return data;
    }
  }

  async createWarehouse(warehouse: any) {
    const url = 'http://localhost:3001/api/warehouse/create';
    const urlBackUp = "http://5.249.66.111:3001/api/warehouse/create";
    const data = warehouse;
    
    try {
      
      const response = fetch(url, {
        method: 'POST',
        body: JSON.stringify(data),
        headers: {
          'Content-Type': 'application/json'
        },

      })
      return response;
      
    } catch {
  
      const response = fetch(urlBackUp, {
        method: 'POST',
        body: JSON.stringify(data),
        headers: {
          'Content-Type': 'application/json'
        },
  
      })
      return response;
        
    }

    
  }

  async getAllWarehouses() {
    const url = 'http://localhost:3001/api/warehouse/all';
    const urlBackUp = "http://5.249.66.111:3001/api/warehouse/all";
    try {
      
      const response = await fetch(url, {
        method: 'GET'
      })
      const data = await response.json();
      return data;
     
    } catch {
    
      const response = await fetch(urlBackUp, {
        method: 'GET'
      })
      const data = await response.json();
      return data;
        
    }
  }

  async updateWarehouse(warehouse: any) {

    const url = 'http://localhost:3001/api/warehouse/update';
    const urlBackUp = "http://5.249.66.111:3001/api/warehouse/update";
    const data = warehouse;

    try {

      const response = await fetch(url, {
        method: 'PUT',
        body: JSON.stringify(data),
        headers: {
          'Content-Type': 'application/json'
        },
      })
      
      return response;
        
    } catch {

      const response = await fetch(urlBackUp, {
        method: 'PUT',
        body: JSON.stringify(data),
        headers: {
          'Content-Type': 'application/json'
        },
      })
      
      return response;
        
    }

  }

  async deleteWarehouse(WarehousId: string) {
    const url = 'http://localhost:3001/api/warehouse/delete/'+WarehousId;
    const urlBackUp = "http://5.249.66.111:3001/api/warehouse/delete/"+WarehousId;
    try {
      const response = await fetch(url, {
        method: 'DELETE',
        headers: {
          'Accept': 'application/json'
        }
      });
      
  
      return response;
        
    } catch  {
      const response = await fetch(urlBackUp, {
        method: 'DELETE',
        headers: {
          'Accept': 'application/json'
        }
      });
      
  
      return response;
        
    }
  }



}
