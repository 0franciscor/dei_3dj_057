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

      let data = await response.json();
      res.status(200);
      return res.json(data);
  }


}