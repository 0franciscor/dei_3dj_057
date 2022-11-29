import { Request, Response, NextFunction } from 'express';

import { Inject, Service } from 'typedi';
import config from "../../config";

import IWarehouseController from "./IControllers/IWarehouseController";

import { Result } from "../core/logic/Result";
import { Console } from 'console';

import fetch from 'node-fetch';
const http = require('https');

@Service()
export default class WarehouseController implements IWarehouseController {
  constructor() {}


  public async getAllWarehouse(req: Request, res: Response, next: NextFunction){
    
    const httpAgent = new http.Agent({ rejectUnauthorized: false });
    const address = 'https://localhost:5001/api/warehouses/GetAll';

    
    const response = await fetch(address, {
        method: 'GET',
        agent: httpAgent
    });
    
    
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
    const address = 'https://localhost:5001/api/warehouses/GetAllCities';

    
    const response = await fetch(address, {
        method: 'GET',
        agent: httpAgent
    });
    
    
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
    const address = 'https://localhost:5001/api/warehouses/CreateWarehouse';

      const response = await fetch(address,{
          method: 'POST',
          body: JSON.stringify(req.body),
          headers: { 'Content-Type': 'application/json' },
          agent: httpAgent
      });

      if(response.status != 200){
        res.status(response.status);
        return res.json({message: "Error creating warehouse"});
      }

      const address_prolog = 'https://vs-gate.dei.isep.ipp.pt:30382/create_warehouse';

      const response_prolog = await fetch(address_prolog,{
          method: 'POST',
          body: JSON.stringify(response.body),
          headers: { 'Content-Type': 'application/json' },
          agent: httpAgent
      });

      const info = await response.json();
      res.status(201);
      return res.json(info);
  }

  async getWarehouse(req: Request, res: Response, next: NextFunction) {
    const httpAgent = new http.Agent({ rejectUnauthorized: false });

    const url = 'https://localhost:5001/api/warehouses/GetById/'+req.params.id;

    const response = await fetch(url, {
      method: 'GET',
      headers: {
        'Accept': 'application/json',
      },
      agent: httpAgent
    });
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

    const url = 'https://localhost:5001/api/warehouses/Update';
    
    const response = await fetch(url, {
      method: 'PUT',
      body: JSON.stringify(req.body),
      headers: {
        'Content-Type': 'application/json'
      },
      agent: httpAgent
    })

    if(response.status != 200){
      res.status(response.status);
      return res.json({message: "Error updating warehouse"});
    }

    const url_prolog = 'https://vs-gate.dei.isep.ipp.pt:30382/create_warehouse';
    
    const response_prolog = await fetch(url_prolog, {
      method: 'PUT',
      body: JSON.stringify(req.body),
      headers: {
        'Content-Type': 'application/json'
      },
      agent: httpAgent
    })

    const info = await response.json();
    res.status(200);
    return res.json(info);
  }



}