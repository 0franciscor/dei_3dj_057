import { Request, Response, NextFunction } from 'express';
import { Service } from 'typedi';

import IDeliveryController from "./IControllers/IDeliveryController";

import fetch from 'node-fetch';
import config from '../../config';

const http = require('https');

const jwt = require('jsonwebtoken');
@Service()
export default class DeliveryController implements IDeliveryController {
    constructor() { }

    private roles = ["admin","whMan"];

    isAuthenticated(req: Request) {
        if(req.cookies['jwt'] == undefined)
        return false;
        const cookie = req.cookies['jwt'];
        const claims = jwt.verify(cookie, config.jwtSecret);
        if(!claims)
            return false;
        
        return true;
    }

    isAuthorized(req: Request) {
        if(req.cookies['jwt'] == undefined)
        return false;
        const cookie = req.cookies['jwt'];
        const claims = jwt.verify(cookie, config.jwtSecret);
        if(!claims)
            return false;
        if(this.roles.indexOf(claims.role) > -1)
            return true;
        return false;
    }

    public async getAllDeliveries(req: Request, res: Response, next: NextFunction) {
      if(req.headers.authorization!=undefined)
      req.cookies["jwt"]=req.headers.authorization.split("=")[1];
    if(!this.isAuthenticated(req)){
      res.status(401);
      return res.json({message: "Not authenticated"});
    }
    if(!this.isAuthorized(req)){
      res.status(403);
      return res.json({message: "Not authorized"});
    }
    req.headers.cookie = "jwt="+req.cookies["jwt"];

        const httpAgent = new http.Agent({ rejectUnauthorized: false });
        let address = 'https://localhost:5001/api/deliveries/GetAll';
        if(req.get('host').includes("azure"))
          address = 'https://whmanagement57.azurewebsites.net/api/deliveries/GetAll/';

        const response = await fetch(address, {
            method: 'GET',
            agent: httpAgent,
            headers: {
                'Content-Type': 'application/json',
                'Cookie': req.headers.cookie
            },
            
        });

        if (response.status != 200) {
            res.status(response.status);
            return res.json({ message: "Error Getting Deliveries" });
        }
        const info = await response.json();
        res.status(200);
        return res.json(info);
    }

    public async getAllDeliveriesProlog(req: Request, res: Response, next: NextFunction) {
      if(req.headers.authorization!=undefined)
      req.cookies["jwt"]=req.headers.authorization.split("=")[1];
    if(!this.isAuthenticated(req)){
      res.status(401);
      return res.json({message: "Not authenticated"});
    }
    if(!this.isAuthorized(req)){
      res.status(403);
      return res.json({message: "Not authorized"});
    }
    req.headers.cookie = "jwt="+req.cookies["jwt"];
        const httpAgent = new http.Agent({ rejectUnauthorized: false });
        const address = 'https://localhost:5001/api/deliveries/GetAllProlog';

        const response = await fetch(address, {
            method: 'GET',
            agent: httpAgent,
            headers: {
                'Content-Type': 'application/json',
                'Cookie': req.headers.cookie
              },
        });

        if (response.status != 200) {
            res.status(response.status);
            return res.json({ message: "Error Getting Deliveries" });
        }
        const info = await response.json();
        res.status(200);
        return res.json(info);
    }

    public async getDelivery(req: Request, res: Response, next: NextFunction) {
      if(req.headers.authorization!=undefined)
      req.cookies["jwt"]=req.headers.authorization.split("=")[1];
    if(!this.isAuthenticated(req)){
      res.status(401);
      return res.json({message: "Not authenticated"});
    }
    if(!this.isAuthorized(req)){
      res.status(403);
      return res.json({message: "Not authorized"});
    }
    req.headers.cookie = "jwt="+req.cookies["jwt"];
        const httpAgent = new http.Agent({ rejectUnauthorized: false });
        let address = 'https://localhost:5001/api/deliveries/GetByID/' + req.params.id;
        if(req.get('host').includes("azure"))
          address = 'https://whmanagement57.azurewebsites.net/api/deliveries/GetByID/'+ req.params.id;

        const response = await fetch(address, {
            method: 'GET',
            agent: httpAgent,
            headers: {
                'Content-Type': 'application/json',
                'Cookie': req.headers.cookie
              },
        });

        if (response.status != 200) {
            res.status(response.status);
            return res.json({ message: "Error Getting Delivery" });
        }
        const info = await response.json();
        res.status(200);
        return res.json(info);
    }

    public async createDelivery(req: Request, res: Response, next: NextFunction) {
      if(req.headers.authorization!=undefined)
      req.cookies["jwt"]=req.headers.authorization.split("=")[1];
    if(!this.isAuthenticated(req)){
      res.status(401);
      return res.json({message: "Not authenticated"});
    }
    if(!this.isAuthorized(req)){
      res.status(403);
      return res.json({message: "Not authorized"});
    }
    req.headers.cookie = "jwt="+req.cookies["jwt"];
        const httpAgent = new http.Agent({ rejectUnauthorized: false });
        let address = 'https://localhost:5001/api/deliveries/CreateDelivery';

        if(req.get('host').includes("azure"))
            address = 'https://whmanagement57.azurewebsites.net/api/deliveries/CreateDelivery/';
        const response = await fetch(address, {
            method: 'POST',
            body: JSON.stringify(req.body),
            headers: {
                'Content-Type': 'application/json',
                'Cookie': req.headers.cookie
              },
            agent: httpAgent
        });

        if (response.status != 200) {
            res.status(response.status);
            return res.json({ message: "Error Creating Delivery"});
        }

        const info = await response.json();
        res.status(200);
        return res.json(info);
    };

    public async createDeliveryProlog(req: Request, res: Response, next: NextFunction) {
      if(req.headers.authorization!=undefined)
        req.cookies["jwt"]=req.headers.authorization.split("=")[1];
      if(!this.isAuthenticated(req)){
        res.status(401);
        return res.json({message: "Not authenticated"});
      }
      if(!this.isAuthorized(req)){
        res.status(403);
        return res.json({message: "Not authorized"});
      }
      req.headers.cookie = "jwt="+req.cookies["jwt"];
        const httpAgent = new http.Agent({ rejectUnauthorized: false });
        const address_prolog = 'https://vs-gate.dei.isep.ipp.pt:30382/create_delivery';
        const response_prolog = await fetch(address_prolog, {
            method: 'POST',
            body: JSON.stringify(req.body),
            headers: {
                'Content-Type': 'application/json',
                'Cookie': req.headers.cookie
              },
            agent: httpAgent
        });

        if (response_prolog.status != 200) {
            res.status(response_prolog.status);
            return res.json({ message: "Error Creating Delivery"});
        }

        const info = await response_prolog.json();
        res.status(200);
        return res.json(info);
    };


    public async updateDelivery(req: Request, res: Response, next: NextFunction) {
      if(req.headers.authorization!=undefined)
      req.cookies["jwt"]=req.headers.authorization.split("=")[1];
    if(!this.isAuthenticated(req)){
      res.status(401);
      return res.json({message: "Not authenticated"});
    }
    if(!this.isAuthorized(req)){
      res.status(403);
      return res.json({message: "Not authorized"});
    }
    req.headers.cookie = "jwt="+req.cookies["jwt"];
        const httpAgent = new http.Agent({ rejectUnauthorized: false });
        let address = 'https://localhost:5001/api/deliveries/Update';
        if(req.get('host').includes("azure"))
          address = 'https://whmanagement57.azurewebsites.net/api/deliveries/Update/';

        const response = await fetch(address, {
            method: 'PATCH',
            body: JSON.stringify(req.body),
            headers: {
                'Content-Type': 'application/json',
                'Cookie': req.headers.cookie
              },
            agent: httpAgent
        });

        if (response.status != 200) {
            res.status(response.status);
            return res.json({ message: "Error Updating Delivery" });
        }

        const info = await response.json();
        res.status(200);
        return res.json(info);
    }

    public async updateDeliveryProlog(req: Request, res: Response, next: NextFunction) {
      if(req.headers.authorization!=undefined)
      req.cookies["jwt"]=req.headers.authorization.split("=")[1];
    if(!this.isAuthenticated(req)){
      res.status(401);
      return res.json({message: "Not authenticated"});
    }
    if(!this.isAuthorized(req)){
      res.status(403);
      return res.json({message: "Not authorized"});
    }
    req.headers.cookie = "jwt="+req.cookies["jwt"];
        const httpAgent = new http.Agent({ rejectUnauthorized: false });

        const address_prolog = 'https://vs-gate.dei.isep.ipp.pt:30382/update_delivery';

        const response_prolog = await fetch(address_prolog, {
            method: 'PUT',
            body: JSON.stringify(req.body),
            headers: {
                'Content-Type': 'application/json',
                'Cookie': req.headers.cookie
              },
            agent: httpAgent
        });

        if (response_prolog.status != 200) {
            res.status(response_prolog.status);
            return res.json({ message: "Error Updating Delivery" });
        }
        
        const info = await response_prolog.json();
        res.status(200);
        return res.json(info);
    }

    public async getDeliveryDestination(req: Request, res: Response, next: NextFunction){
      if(req.headers.authorization!=undefined)
      req.cookies["jwt"]=req.headers.authorization.split("=")[1];
    if(!this.isAuthenticated(req)){
      res.status(401);
      return res.json({message: "Not authenticated"});
    }
    if(!this.isAuthorized(req)){
      res.status(403);
      return res.json({message: "Not authorized"});
    }
    req.headers.cookie = "jwt="+req.cookies["jwt"];
        let deliveredWarehouseList :any[]=[]
        let deliveriesMoved : any[]=[]
        

        let plan = {
            truck : "eTruck01",
            info:[]
            
        }
        
        const httpAgent = new http.Agent({ rejectUnauthorized: false });
       //GET DELIVERIES
        let address = 'https://localhost:5001/api/deliveries/GetAll';
        if(req.get('host').includes("azure"))
          address = 'https://whmanagement57.azurewebsites.net/api/deliveries/GetAll/';

        const responseDeliveries = await fetch(address, {
            method: 'GET',
            agent: httpAgent,
            headers: {
                'Content-Type': 'application/json',
                'Cookie': req.headers.cookie
              },
        });

        if (responseDeliveries.status != 200) {
            res.status(responseDeliveries.status);
            return res.json({ message: "Error Getting Deliveries" });
        }
        const deliveries = await responseDeliveries.json();

        // GET WAREHOUSES
        const warehousesAddress = 'https://localhost:5001/api/warehouses/GetAll'
        const responseWarehouse = await fetch(warehousesAddress, {
            method: 'GET',
            agent: httpAgent,
            headers: {
                'Content-Type': 'application/json',
                'Cookie': req.headers.cookie
              },
        });
        if (responseWarehouse.status != 200) {
            res.status(responseWarehouse.status);
            return res.json({ message: "Error Getting Warehouses" });
        }
        const warehouses= await responseWarehouse.json()
        for(let j=0; j< req.body.pathList.length; j++)
            for(let i=0;i< warehouses.length; i++ ){
                if(warehouses[i].city == req.body.pathList[j]){
                    deliveredWarehouseList.push(warehouses[i]);
                }
            }
            
          
            for(let x =0 ;x< req.body.pathList.length; x++){
                for( let y=0; y< deliveries.length ;y++){
                   let firstsplit= deliveries[y].deliveryDate.split('T');
                   
                    let secondSplit= firstsplit[0].split('-');
                    if( secondSplit[2].charAt(0)=='0'){
                        secondSplit[2]= secondSplit[2].charAt(1)
                    }
                    if(deliveries[y].destination == req.body.pathList[x] && req.body.date== secondSplit[0]+secondSplit[1]+secondSplit[2]){
                        
                        deliveriesMoved.push(deliveries[y])
                    }
                }
        }
        console.log(deliveredWarehouseList)
        console.log(deliveriesMoved)
        // for(let l=1; l<deliveredWarehouseList.length-1; l++){
          

        //     plan.info.push({
        //         warehouse: deliveredWarehouseList[l].id,
        //         delivery: deliveriesMoved[l-1].deliveryID
        //     })
        // }
        
        deliveredWarehouseList.forEach(deliveredWarehouse => {
          deliveriesMoved.forEach(delivery => {
            if(deliveredWarehouse.active &&deliveredWarehouse.city == delivery.destination ){
              plan.info.push({
                warehouse: deliveredWarehouse.id,
                delivery: delivery.deliveryID
              })
            }
          });
        });
        // console.log(plan)
        res.status(200);
        return res.json(plan);
    }
}