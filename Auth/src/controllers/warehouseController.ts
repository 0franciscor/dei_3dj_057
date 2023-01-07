import { Request, Response, NextFunction } from 'express';
import { Inject, Service } from 'typedi';
import IWarehouseController from "./IControllers/IWarehouseController";
import fetch from 'node-fetch';
import config from '../../config';
import { ParamsDictionary } from 'express-serve-static-core';
import { ParsedQs } from 'qs';
const http = require('https');
const jwt = require('jsonwebtoken');
@Service()
export default class WarehouseController implements IWarehouseController {
  constructor() { }

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

  public async getAllWarehouse(req: Request, res: Response, next: NextFunction) {

    //################################

    /* if (req.headers.origin != undefined) {
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
    } //Commented due to prolog */

    //################################

    const httpAgent = new http.Agent({ rejectUnauthorized: false });

    let address = 'https://localhost:5001/api/warehouses/GetAll';
    const host = req.get('host');
    if (typeof host === 'string' && host.includes("azure"))
      address = 'https://whmanagement57.azurewebsites.net/api/warehouses/GetAll/';
    const response = await this.fetch(address, 'GET', null, req.headers.cookie, httpAgent);

    if (response.status != 200) {
      res.status(response.status);
      return res.json({ message: "Error Getting Warehouses" });
    }
    const info = await response.json();
    res.status(200);

    return res.json(info);
  }

  async getWarehouse(req: Request, res: Response, next: NextFunction) {
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

    let url = 'https://localhost:5001/api/warehouses/GetById/' + req.params.id;
    const host = req.get('host');
    if (typeof host === 'string' && host.includes("azure"))
      url = 'https://whmanagement57.azurewebsites.net/api/warehouses/GetById/' + req.params.id;

    const response = await this.fetch(url, 'GET', null, req.headers.cookie, httpAgent);

    if (response.status != 200) {
      res.status(response.status);
      return res.json({ message: "Error getting warehouse" });
    }

    const data = await response.json();
    res.status(200)

    return res.json(data);
  }

  public async createWarehouse(req: Request, res: Response, next: NextFunction) {

    //############################################

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

    //###########################################


    let address = 'https://localhost:5001/api/warehouses/CreateWarehouse';
    const host = req.get('host');
    if (typeof host === 'string' && host.includes("azure"))
      address = 'https://whmanagement57.azurewebsites.net/api/warehouses/CreateWarehouse/';

    const data = req.body;

    const response = await this.fetch(address, 'POST', data, req.headers.cookie, httpAgent);

    if (response.status != 200) {
      res.status(response.status);
      return res.json({ message: "Error creating warehouse" });
    }

    const info = await response.json();
    res.status(201);

    return res.json(info);
  }

  async editWarehouse(req: Request, res: Response, next: NextFunction) {
    
    //############################################

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

    //############################################

    let url = 'https://localhost:5001/api/warehouses/Update';
    const host = req.get('host');
    if (typeof host === 'string' && host.includes("azure"))
      url = 'https://whmanagement57.azurewebsites.net/api/warehouses/Update';


    const data = req.body;


    const response = await this.fetch(url, 'PUT', data, req.headers.cookie, httpAgent);

    if (response.status != 200) {
      res.status(response.status);
      return res.json({ message: "Error updating warehouse" });
    }

    const info = await response.json();
    res.status(200);
    return res.json(info);
  }

  async activateWarehouse(req: Request, res: Response, next: NextFunction) {

    //############################################

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

    //############################################

    let url = 'https://localhost:5001/api/warehouses/Activate/' + req.params.id;

    const host = req.get('host');
    if (typeof host === 'string' && host.includes("azure"))
      url = 'https://whmanagement57.azurewebsites.net/api/warehouses/Activate' + req.params.id;

    const response = await this.fetch(url, 'PATCH', null, req.headers.cookie, httpAgent);

    if (response.status != 200) {
      res.status(response.status);
      return res.json({ message: "Error getting warehouse" });
    }

    const data = await response.json();
    res.status(200)
    return res.json(data);
  }

  async deactivateWarehouse(req: Request, res: Response, next: NextFunction) {

    //############################################

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

    //############################################

    let url = 'https://localhost:5001/api/warehouses/Delete/' + req.params.id;

    const host = req.get('host');
    if (typeof host === 'string' && host.includes("azure"))
      url = 'https://whmanagement57.azurewebsites.net/api/warehouses/Delete' + req.params.id;

    const response = await this.fetch(url, 'DELETE', null, req.headers.cookie, httpAgent);

    if (response.status != 200) {
      res.status(response.status);
      return res.json({ message: "Error getting warehouse" });
    }

    const data = await response.json();
    res.status(200);

    return res.json(data);
  }


  async deleteWarehouse(req: Request, res: Response, next: NextFunction) {

    //############################################

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

    //############################################

    let url = 'https://localhost:5001/api/warehouses/HardDelete/' + req.params.id;

    const host = req.get('host');
    if (typeof host === 'string' && host.includes("azure"))
      url = 'https://whmanagement57.azurewebsites.net/api/warehouses/HardDelete' + req.params.id;

    const response = await this.fetch(url, 'DELETE', null, req.headers.cookie, httpAgent);

    if (response.status != 200) {
      res.status(response.status);
      return res.json({ message: "Error getting warehouse" });
    }

    const data = await response.json();
    res.status(200);

    return res.json(data);

  }


  async getAllCities(req: Request, res: Response, next: NextFunction) {

    //############################################

    /* if (req.headers.origin != undefined) {
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
    } */

    //###########################################

    const httpAgent = new http.Agent({ rejectUnauthorized: false });
    let address = 'https://localhost:5001/api/warehouses/GetAllCities';
    const host = req.get('host');
    if (typeof host === 'string' && host.includes("azure"))
      address = 'https://whmanagement57.azurewebsites.net/api/warehouses/GetAllCities/';


    const response = await this.fetch(address, 'GET', null, req.headers.cookie, httpAgent);

    if (response.status != 200) {
      res.status(response.status);
      return res.json({ message: "Error Getting Warehouses" });
    }
    const info = await response.json();
    res.status(200);
    
    return res.json(info);
  }





  // Prolog Related

  public async createWarehouseProlog(req: Request, res: Response, next: NextFunction) {

    //############################################

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

    //############################################

    const address_prolog = 'https://vs-gate.dei.isep.ipp.pt:30382/create_warehouse';

    const data = req.body;

    const response = await this.fetch(address_prolog, 'POST', data, req.headers.cookie, httpAgent);

    if (response.status != 200) {
      res.status(response.status);
      return res.json({ message: "Error Creating Warehouse" });
    }

    const info = await response.json();
    res.status(201);

    return res.json(info);
  }

  async updateWarehouseProlog(req: Request, res: Response, next: NextFunction) {

    //############################################

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

    //############################################

    const url_prolog = 'https://vs-gate.dei.isep.ipp.pt:30382/update_warehouse';

    const data = req.body;

    const response = await this.fetch(url_prolog, 'POST', data, req.headers.cookie, httpAgent);

    if (response.status != 200) {
      res.status(response.status);
      return res.json({ message: "Error updating warehouse" });
    }
    const info = await response.json();
    res.status(200);


    return res.json(info);

  }

  async deleteWarehouseProlog(req: Request, res: Response, next: NextFunction) {
    
    //############################################

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

    //############################################

    const address_prolog = 'https://vs-gate.dei.isep.ipp.pt:30382/delete_warehouse';

    const data = req.body;

    const response = await this.fetch(address_prolog, 'POST', data, req.headers.cookie, httpAgent);

    if (response.status != 200) {
      res.status(response.status);
      return res.json({ message: "Error Deleting Warehouse" });
    }

    const info = await response.json();
    res.status(200);

    return res.json(info);

  }

  async createCityProlog(req: Request, res: Response, next: NextFunction) {
    
    //############################################

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

    //############################################

    const url_prolog = 'https://vs-gate.dei.isep.ipp.pt:30382/create_city';

    const data = req.body;

    const response = await this.fetch(url_prolog, 'POST', data, req.headers.cookie, httpAgent);

    if (response.status != 200) {
      res.status(response.status);
      return res.json({ message: "Error Creating City" });
    }
    
    const info = await response.json();
    res.status(201);

    return res.json(info);

  }

}