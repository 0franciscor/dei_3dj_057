import { Injectable } from '@angular/core';

@Injectable({
  providedIn: 'root'
})
export class RoadNetworkService {

  public urlOrigin = window.location.origin.split(":")[0] + ":" + window.location.origin.split(":")[1] + ":3001/";
  constructor() {}


  async getAllWarehouses() {
    const url = this.urlOrigin+"api/warehouse/all";
    const response = await fetch(url, {
      method: 'GET'
    });
    
    let data = await response.json();
    return data;
  }
  

  async getPathBetweenWarehouses(warehouse1: string) {

    const url = this.urlOrigin+"api/path/all/" + warehouse1;
   
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
}