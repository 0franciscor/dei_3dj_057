import { Request, Response, NextFunction } from 'express';
import { Service } from 'typedi';

import IDeliveryController from "./IControllers/IDeliveryController";

import fetch from 'node-fetch';
import config from '../../config';
import { consumers } from 'stream';
import { json } from 'stream/consumers';

const http = require('https');

const jwt = require('jsonwebtoken');
@Service()
export default class DeliveryController implements IDeliveryController {

  public prologURL !: string;

  constructor() {
    this.prologURL = "https://vs-gate.dei.isep.ipp.pt:30382/";
  }

  private roles = ["admin", "whMan"];

  isAuthenticated(req: Request) {
    try {
      if (req.cookies['jwt'] == undefined)
        return false;
      const cookie = req.cookies['jwt'];

      const claims = jwt.verify(cookie, config.jwtSecret);

      if (!claims)
        return false;

      return true;
    } catch (error) {
      return false
    }
  }

  isAuthorized(req: Request, specifiedRoles?: string[]) {
    try {
      if (req.cookies['jwt'] == undefined)
        return false;
      const cookie = req.cookies['jwt'];
      const claims = jwt.verify(cookie, config.jwtSecret);
      if (!claims)
        return false;
      if (specifiedRoles != undefined) {
        if (specifiedRoles.indexOf(claims.role) > -1)
          return true;
        return false;
      }
      else if (this.roles.indexOf(claims.role) > -1)
        return true;
      return false;
    } catch (error) {
      return false;
    }
  }

  private async fetch(url: string, method: string, body: any, cookie: any, agent: any = null) {
    try {
      if (body)
        return await fetch(url, {
          method: method,
          body: JSON.stringify(body),
          headers: {
            'Content-Type': 'application/json',
            'Cookie': cookie
          },
          agent: agent
        });
      else
        return await fetch(url, {
          method: method,
          headers: {
            'Content-Type': 'application/json',
            'Cookie': cookie
          },
          agent: agent
        });
    } catch (error) {
      return { status: 503, json(): any { return { message: "Error connecting to server" } } };
    }

  }

  public async getAllDeliveries(req: Request, res: Response, next: NextFunction) {

    //######################################################

    /* if (req.headers.authorization != undefined)
      req.cookies["jwt"] = req.headers.authorization.split("=")[1];
    if (!this.isAuthenticated(req)) {
      res.status(401);
      return res.json({ message: "Not authenticated" });
    }
    if (!this.isAuthorized(req)) {
      res.status(403);
      return res.json({ message: "Not authorized" });
    }
    req.headers.cookie = "jwt=" + req.cookies["jwt"];

    //Commented Due to Prolog */

    //######################################################

    const httpAgent = new http.Agent({ rejectUnauthorized: false });  

    let address = 'https://localhost:5001/api/deliveries/GetAll';
    if (req.get('host').includes("azure"))
      address = 'https://whmanagement57.azurewebsites.net/api/delveries/GetAll/';

    const response = await this.fetch(address, 'GET', null, req.headers.cookie, httpAgent);

    if (response.status != 200) {
      res.status(response.status);
      return res.json({ message: "Error Getting Deliveries" });
    }
    const info = await response.json();
    res.status(200);
    return res.json(info);
  }

  public async getDelivery(req: Request, res: Response, next: NextFunction) {

    //######################################################

    if (req.headers.authorization != undefined)
      req.cookies["jwt"] = req.headers.authorization.split("=")[1];
    if (!this.isAuthenticated(req)) {
      res.status(401);
      return res.json({ message: "Not authenticated" });
    }
    if (!this.isAuthorized(req)) {
      res.status(403);
      return res.json({ message: "Not authorized" });
    }
    req.headers.cookie = "jwt=" + req.cookies["jwt"];
    const httpAgent = new http.Agent({ rejectUnauthorized: false });

    //######################################################

    let address = 'https://localhost:5001/api/deliveries/GetByID/' + req.params.id;
    if (req.get('host').includes("azure"))
      address = 'https://whmanagement57.azurewebsites.net/api/deliveries/GetByID/' + req.params.id;

    const response = await this.fetch(address, 'GET', null, req.headers.cookie, httpAgent);

    if (response.status != 200) {
      res.status(response.status);
      return res.json({ message: "Error Getting Delivery" });
    }

    const info = await response.json();
    res.status(200);
    return res.json(info);

  }

  public async createDelivery(req: Request, res: Response, next: NextFunction) {

    //######################################################

    if (req.headers.authorization != undefined)
      req.cookies["jwt"] = req.headers.authorization.split("=")[1];
    if (!this.isAuthenticated(req)) {
      res.status(401);
      return res.json({ message: "Not authenticated" });
    }
    if (!this.isAuthorized(req)) {
      res.status(403);
      return res.json({ message: "Not authorized" });
    }
    req.headers.cookie = "jwt=" + req.cookies["jwt"];
    const httpAgent = new http.Agent({ rejectUnauthorized: false });

    //######################################################

    let address = 'https://localhost:5001/api/deliveries/CreateDelivery';

    if (req.get('host').includes("azure"))
      address = 'https://whmanagement57.azurewebsites.net/api/deliveries/CreateDelivery/';

    const response = await this.fetch(address, 'POST', req.body, req.headers.cookie, httpAgent);

    if (response.status != 201) {
      res.status(response.status);
      return res.json({ message: "Error Creating Delivery" });
    }

    const jsonResponse = await response.json();
    res.status(201);

    return res.json(jsonResponse);
  }

  public async updateDelivery(req: Request, res: Response, next: NextFunction) {

    //######################################################

    if (req.headers.authorization != undefined)
      req.cookies["jwt"] = req.headers.authorization.split("=")[1];
    if (!this.isAuthenticated(req)) {
      res.status(401);
      return res.json({ message: "Not authenticated" });
    }
    if (!this.isAuthorized(req)) {
      res.status(403);
      return res.json({ message: "Not authorized" });
    }
    req.headers.cookie = "jwt=" + req.cookies["jwt"];
    const httpAgent = new http.Agent({ rejectUnauthorized: false });

    //######################################################

    let address = 'https://localhost:5001/api/deliveries/Update';
    if (req.get('host').includes("azure"))
      address = 'https://whmanagement57.azurewebsites.net/api/deliveries/Update/';

    const response = await this.fetch(address, 'PATCH', req.body, req.headers.cookie, httpAgent);

    if (response.status != 200) {
      res.status(response.status);
      return res.json({ message: "Error Updating Delivery" });
    }

    const info = await response.json();
    res.status(200);

    return res.json(info);

  }

  public async deleteDelivery(req: Request, res: Response, next: NextFunction) {
    //######################################################

    if (req.headers.authorization != undefined)
      req.cookies["jwt"] = req.headers.authorization.split("=")[1];
    if (!this.isAuthenticated(req)) {
      res.status(401);
      return res.json({ message: "Not authenticated" });
    }
    if (!this.isAuthorized(req)) {
      res.status(403);
      return res.json({ message: "Not authorized" });
    }
    req.headers.cookie = "jwt=" + req.cookies["jwt"];
    const httpAgent = new http.Agent({ rejectUnauthorized: false });

    //######################################################

    let address = 'https://localhost:5001/api/deliveries/Delete/' + req.params.id;
    if (req.get('host').includes("azure"))
      address = 'https://whmanagement57.azurewebsites.net/api/deliveries/Delete/' + req.params.id;

    const response = await this.fetch(address, 'DELETE', null, req.headers.cookie, httpAgent);

    if (response.status != 200) {
      res.status(response.status);
      return res.json({ message: "Error Deleting Delivery" });
    }
    const info = await response.json();

    res.status(200);

    return res.json(info);
  }

  public async getAllDeliveriesProlog(req: Request, res: Response, next: NextFunction) {

    const httpAgent = new http.Agent({ rejectUnauthorized: false });
    const address = 'https://localhost:5001/api/deliveries/GetAllProlog';

    const response = await this.fetch(address, 'GET', null, req.headers.cookie, httpAgent);

    if (response.status != 200) {
      res.status(response.status);
      return res.json({ message: "Error Getting Deliveries" });
    }
    const info = await response.json();
    res.status(200);
    
    return res.json(info);
  }

  public async createDeliveryProlog(req: Request, res: Response) {

    //######################################################

    if (req.headers.authorization != undefined)
      req.cookies["jwt"] = req.headers.authorization.split("=")[1];
    if (!this.isAuthenticated(req)) {
      res.status(401);
      return res.json({ message: "Not authenticated" });
    }
    if (!this.isAuthorized(req)) {
      res.status(403);
      return res.json({ message: "Not authorized" });
    }
    req.headers.cookie = "jwt=" + req.cookies["jwt"];
    const httpAgent = new http.Agent({ rejectUnauthorized: false });

    //######################################################

    const address_prolog = this.prologURL + 'create_delivery';
    const response_prolog = await this.fetch(address_prolog, 'POST', req.body, req.headers.cookie, httpAgent);

    if (response_prolog.status != 200) {
      res.status(response_prolog.status);
      return res.json({ message: "Error Creating Delivery on the Prolog Server" });
    }

    const info = await response_prolog.json();
    res.status(200);

    return res.json(info);
  };

  public async updateDeliveryProlog(req: Request, res: Response) {
    //######################################################

    if (req.headers.authorization != undefined)
      req.cookies["jwt"] = req.headers.authorization.split("=")[1];
    if (!this.isAuthenticated(req)) {
      res.status(401);
      return res.json({ message: "Not authenticated" });
    }
    if (!this.isAuthorized(req)) {
      res.status(403);
      return res.json({ message: "Not authorized" });
    }
    req.headers.cookie = "jwt=" + req.cookies["jwt"];
    const httpAgent = new http.Agent({ rejectUnauthorized: false });

    //######################################################

    const address_prolog = this.prologURL + 'update_delivery';

    const response_prolog = await this.fetch(address_prolog, 'POST', req.body, req.headers.cookie, httpAgent);

    if (response_prolog.status != 200) {
      res.status(response_prolog.status);
      return res.json({ message: "Error Updating Delivery" });
    }

    const info = await response_prolog.json();
    res.status(200);

    return res.json(info);
  }

  public async deleteDeliveryProlog(req: Request, res: Response) {
    
    //######################################################

    if (req.headers.authorization != undefined)
      req.cookies["jwt"] = req.headers.authorization.split("=")[1];
    if (!this.isAuthenticated(req)) {
      res.status(401);
      return res.json({ message: "Not authenticated" });
    }
    if (!this.isAuthorized(req)) {
      res.status(403);
      return res.json({ message: "Not authorized" });
    }
    req.headers.cookie = "jwt=" + req.cookies["jwt"];
    const httpAgent = new http.Agent({ rejectUnauthorized: false });

    //######################################################

    const address_prolog = this.prologURL + 'delete_delivery';

    const response_prolog = await this.fetch(address_prolog, 'POST', req.body, req.headers.cookie, httpAgent);

    /* if (response_prolog.status != 200) {
      res.status(response_prolog.status);
      return res.json({ message: "Error Updating Delivery" });
    } */

    const info = await response_prolog.json();
    res.status(200);

    return res.json(info);
  }


  public async getDeliveryDestination(req: Request, res: Response, next: NextFunction) {
    if (req.headers.authorization != undefined)
      req.cookies["jwt"] = req.headers.authorization.split("=")[1];
    if (!this.isAuthenticated(req)) {
      res.status(401);
      return res.json({ message: "Not authenticated" });
    }
    if (!this.isAuthorized(req)) {
      res.status(403);
      return res.json({ message: "Not authorized" });
    }
    req.headers.cookie = "jwt=" + req.cookies["jwt"];
    let deliveredWarehouseList: any[] = []
    let deliveriesMoved: any[] = []


    let plan = {
      truck: "eTruck01",
      info: []

    }

    const httpAgent = new http.Agent({ rejectUnauthorized: false });
    //GET DELIVERIES
    let address = 'https://localhost:5001/api/deliveries/GetAll';
    if (req.get('host').includes("azure"))
      address = 'https://whmanagement57.azurewebsites.net/api/deliveries/GetAll/';

    const responseDeliveries = await this.fetch(address, 'GET', null, req.headers.cookie, httpAgent);

    if (responseDeliveries.status != 200) {
      res.status(responseDeliveries.status);
      return res.json({ message: "Error Getting Deliveries" });
    }
    const deliveries = await responseDeliveries.json();

    // GET WAREHOUSES
    const warehousesAddress = 'https://localhost:5001/api/warehouses/GetAll'

    const responseWarehouse = await this.fetch(warehousesAddress, 'GET', null, req.headers.cookie, httpAgent);
    if (responseWarehouse.status != 200) {
      res.status(responseWarehouse.status);
      return res.json({ message: "Error Getting Warehouses" });
    }
    const warehouses = await responseWarehouse.json()
    for (let j = 0; j < req.body.pathList.length; j++)
      for (let i = 0; i < warehouses.length; i++) {
        if (warehouses[i].city == req.body.pathList[j]) {
          deliveredWarehouseList.push(warehouses[i]);
        }
      }


    for (let x = 0; x < req.body.pathList.length; x++) {
      for (let y = 0; y < deliveries.length; y++) {
        let firstsplit = deliveries[y].deliveryDate.split('T');

        let secondSplit = firstsplit[0].split('-');
        if (secondSplit[2].charAt(0) == '0') {
          secondSplit[2] = secondSplit[2].charAt(1)
        }
        if (deliveries[y].destination == req.body.pathList[x] && req.body.date == secondSplit[0] + secondSplit[1] + secondSplit[2]) {

          deliveriesMoved.push(deliveries[y])
        }
      }
    }

    deliveredWarehouseList.forEach(deliveredWarehouse => {
      deliveriesMoved.forEach(delivery => {
        if (deliveredWarehouse.active && deliveredWarehouse.city == delivery.destination) {
          plan.info.push({
            warehouse: deliveredWarehouse.id,
            delivery: delivery.deliveryID
          })
        }
      });
    });
    res.status(200);
    return res.json(plan);
  }

}