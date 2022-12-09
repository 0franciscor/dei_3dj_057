import { Injectable } from '@angular/core';

@Injectable({
  providedIn: 'root'
})
export class RoadNetworkService {

  public urlOrigin = window.location.origin.split(":")[0] + ":" + window.location.origin.split(":")[1] + ":3001/";
  constructor() {}


  async getAllWarehouses() {
    let url = this.urlOrigin+"api/warehouse/all";
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/warehouse/all';
    }
    const response = await fetch(url, {
      method: 'GET',
      headers:{
        'Accept': 'application/json',
        "authorization": document.cookie,
      }
    });
    
    let data = await response.json();
    return data;
  }
  

  async getPathBetweenWarehouses(warehouse1: string) {

    let url = this.urlOrigin+"api/path/all/" + warehouse1+"/undefined";
    if(this.urlOrigin.includes("azure")){
      url = 'https://auth57.azurewebsites.net/api/path/all/'+warehouse1+"/undefined";
    }
    
    const response = await fetch(url, {
      method: 'GET',
      headers:{
        'Accept': 'application/json',
        "authorization": document.cookie,
      }
    });
    
    if(response.status == 404){
      return null;
    }else{
      
      let data = await response.json();
      
      return data;

    }

    

  }
}