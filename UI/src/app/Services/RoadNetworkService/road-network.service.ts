import { Injectable } from '@angular/core';

@Injectable({
  providedIn: 'root'
})
export class RoadNetworkService {

  constructor() {}


  async getAllWarehouses() {
    const url = 'http://localhost:3001/api/warehouse/all';
    const urlBackUp = "http://5.249.66.111:3001/api/warehouse/all";

    try{
      
      const response = await fetch(url, {
        method: 'GET'
      });
      
      let data = await response.json();
      return data;
    }
    catch{
      
      const response = await fetch(urlBackUp, {
        method: 'GET'
      });
      
      let data = await response.json();
      return data;
    }
  }

  async getPathBetweenWarehouses(warehouse1: string) {

    const url = 'http://localhost:3001/api/path/all/' + warehouse1;
    const urlBackUp = "http://5.249.66.111:3001/api/path/all/"+warehouse1;
    try{
        
      const response = await fetch(url, {
        method: 'GET'
      });
      if(response.status == 404){
        return null;
      }else{
        
        let data = await response.json();
        
        return data;

      }

    }
    catch{
        
      const response = await fetch(urlBackUp, {
        method: 'GET'
      });
      if(response.status == 404){
        return null;
      }else{
        
        let data = await response.json();
        
        return data;

      }
      
    }

  }
}