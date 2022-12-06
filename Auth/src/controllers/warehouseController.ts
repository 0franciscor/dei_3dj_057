import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import IWarehouseController from "./IControllers/IWarehouseController";
import fetch from 'node-fetch';
const http = require('https');

@Service()
export default class WarehouseController implements IWarehouseController {
  constructor() {}

  private async fetch(url : string, method: string, body: any, agent: any = null){
   
    if(body)
      return await fetch(url,{
        method : method,
        body : JSON.stringify(body),
        headers: {
          'Content-Type': 'application/json'
        },
        agent: agent
      });
    else
      return await fetch(url,{
        method : method,
        headers: {
          'Content-Type': 'application/json'
        },
        agent: agent
      });
  }

  public async getAllWarehouse(req: Request, res: Response, next: NextFunction){
    
    const httpAgent = new http.Agent({ rejectUnauthorized: false });
    let address = 'https://localhost:5001/api/warehouses/GetAll';
    if(req.get('host').includes("azure"))
      address = 'https://whmanagement57.azurewebsites.net/api/warehouses/GetAll/';
    const response = await this.fetch(address, 'GET', null, httpAgent); 
    
    if (response.status != 200) {
      res.status(response.status);
      return res.json({ message: "Error Getting Warehouses" });
    }
    const info = await response.json();
    res.status(200);
    return res.json(info);

  }

  public async getAllCities(req: Request, res: Response, next: NextFunction){
    
    const httpAgent = new http.Agent({ rejectUnauthorized: false });
    let address = 'https://localhost:5001/api/warehouses/GetAllCities';
    if(req.get('host').includes("azure"))
      address = 'https://whmanagement57.azurewebsites.net/api/warehouses/GetAllCities/';

    
    const response = await this.fetch(address, 'GET', null, httpAgent); 
    
    if (response.status != 200) {
      res.status(response.status);
      return res.json({ message: "Error Getting Warehouses" });
    }
    const info = await response.json();
    res.status(200);
    return res.json(info);

  }

  public async createWarehouse(req: Request, res: Response, next: NextFunction){

    const httpAgent = new http.Agent({ rejectUnauthorized: false });
    let address = 'https://localhost:5001/api/warehouses/CreateWarehouse';
    if(req.get('host').includes("azure"))
      address = 'https://whmanagement57.azurewebsites.net/api/warehouses/CreateWarehouse/';

    const data = req.body;

    const response = await this.fetch(address, 'POST', data, httpAgent); 

    if(response.status != 200){
      res.status(response.status);
      return res.json({message: "Error creating warehouse"});
    }

    const info = await response.json();
    res.status(201);
    return res.json(info);
  }

    public async createWarehouseProlog(req: Request, res: Response, next: NextFunction){

      const httpAgent = new http.Agent({ rejectUnauthorized: false });

      const address_prolog = 'https://vs-gate.dei.isep.ipp.pt:30382/create_warehouse';

      const data = req.body;

      const response = await this.fetch(address_prolog, 'POST', data, httpAgent); 

      const info = await response.json();
      res.status(201);
      return res.json(info);
    }

  async getWarehouse(req: Request, res: Response, next: NextFunction) {
    const httpAgent = new http.Agent({ rejectUnauthorized: false });

    let url = 'https://localhost:5001/api/warehouses/GetById/'+req.params.id;
    
    if(req.get('host').includes("azure"))
      url = 'https://whmanagement57.azurewebsites.net/api/warehouses/GetById/'+req.params.id;

    const response = await this.fetch(url, 'GET', null, httpAgent);

    if(response.status != 200){
      res.status(response.status);
      return res.json({message: "Error getting warehouse"});
    }

    const data = await response.json();
    res.status(200)
    return res.json(data);
  }

  async editWarehouse(req: Request, res: Response, next: NextFunction) {
    const httpAgent = new http.Agent({ rejectUnauthorized: false });

    let url = 'https://localhost:5001/api/warehouses/Update';
    if(req.get('host').includes("azure"))
      url = 'https://whmanagement57.azurewebsites.net/api/warehouses/Update/';
    
    const response = await this.fetch(url, 'PUT', null, httpAgent);

    if(response.status != 200){
      res.status(response.status);
      return res.json({message: "Error updating warehouse"});
    }

    const info = await response.json();
    res.status(200);
    return res.json(info);
  }

  async editWarehouseProlog(req: Request, res: Response, next: NextFunction) {
    const httpAgent = new http.Agent({ rejectUnauthorized: false });

    const url_prolog = 'https://vs-gate.dei.isep.ipp.pt:30382/create_warehouse';
    
    const response = await this.fetch(url_prolog, 'PUT', null, httpAgent);

    if(response.status != 200){
      res.status(response.status);
      return res.json({message: "Error updating warehouse"});
    }
    const info = await response.json();
    res.status(200);
    return res.json(info);
  }

}