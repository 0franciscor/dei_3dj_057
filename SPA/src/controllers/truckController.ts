import { Request, Response, NextFunction } from 'express';

import { Inject, Service } from 'typedi';
import config from "../../config";

import ITruckController from "./IControllers/ITruckController";

import { Result } from "../core/logic/Result";
import { Console } from 'console';

import fetch from 'node-fetch';
const http = require('https');

@Service()
export default class TruckController implements ITruckController {
  constructor() {}

  async createTruck(req: Request, res: Response, next: NextFunction) {
    const url = 'http://localhost:3000/api/truck/';
    const data = req.body;
    const response = await fetch(url, {
      method: 'POST',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },
      // agent: httpAgent
    })
    return await response.json();

  }

  async getAllTruck(req: Request, res: Response, next: NextFunction) {
    const url = 'http://localhost:3000/api/truck/all';
    const response = await fetch(url, {
      method: 'GET',
      headers: {
        'Accept': 'application/json'
      }
    });

    const data = await response.json();
    res.status(200);
    return res.json(data);
  }

  async getTruck(req: Request, res: Response, next: NextFunction) {
    const url = 'http://localhost:3000/api/truck/id/'+req.body.truckId;
    const response = await fetch(url, {
      method: 'GET',
      headers: {
        'Accept': 'application/json'
      }
    });

    const data = await response.json();
    res.status(200)
    return res.json(data);
  }

  async editTruck(req: Request, res: Response, next: NextFunction) {
    const url = 'http://localhost:3000/api/truck/';
    
    const data = req.body;
    console.log(data)
    const response = await fetch(url, {
      method: 'PATCH',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },
    })
    const info = await response.json();
    res.status(200);
    return res.json(info);
  }

  async deleteTruck(req: Request, res: Response, next: NextFunction) {
    const url = 'http://localhost:3000/api/truck/';
    const data = req.body;
    const response = await fetch(url, {
      method: 'DELETE',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },
      // agent: httpAgent
    })
    console.log(response);
  }

 



}