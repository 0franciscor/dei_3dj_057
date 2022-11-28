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
    
    
    let data = await response.json();
    res.status(200)
    return res.json(data);

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
      const info = await response.json();
      res.status(201);
      return res.json(info);
  }

  async getWarehouse(req: Request, res: Response, next: NextFunction) {
    const httpAgent = new http.Agent({ rejectUnauthorized: false });

    const url = 'https://localhost:5001/api/warehouses/GetById/'+req.body.warehouseId;

    const response = await fetch(url, {
      method: 'GET',
      headers: {
        'Accept': 'application/json',
        agent: httpAgent
      }
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
    
    const data = req.body;
    const response = await fetch(url, {
      method: 'PUT',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },
      agent: httpAgent
    })

    if(response.status != 200){
      res.status(response.status);
      return res.json({message: "Error editing truck"});
    }

    const info = await response.json();
    res.status(200);
    return res.json(info);
  }



}