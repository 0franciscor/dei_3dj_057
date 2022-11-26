import { Request, Response, NextFunction } from 'express';
import { Service } from 'typedi';

import IDeliveryController from "./IControllers/IDeliveryController";

import fetch from 'node-fetch';

const http = require('https');

@Service()
export default class DeliveryController implements IDeliveryController {
    constructor() { }

    public async createDelivery(req: Request, res: Response, next: NextFunction) {
        const httpAgent = new http.Agent({ rejectUnauthorized: false });
        const address = 'https://localhost:5001/api/deliveries/CreateDelivery';

        const response = await fetch(address,{
            method: 'POST',
            body: JSON.stringify(req.body),
            headers: { 'Content-Type': 'application/json' },
            agent: httpAgent
        });
        
        let data = await response.json();
        res.status(200);
        return res.json(data);
    };


    public async getAllDeliveries(req: Request, res: Response, next: NextFunction) {

        const httpAgent = new http.Agent({ rejectUnauthorized: false });
        const address = 'https://localhost:5001/api/deliveries/GetAll';

        const response = await fetch(address, {
            method: 'GET',
            agent: httpAgent
        });

        let data = await response.json();
        res.status(200)
        return res.json(data);
    }
    

    public async updateDelivery(req: Request, res: Response, next: NextFunction) {
        const httpAgent = new http.Agent({ rejectUnauthorized: false });
        const address = 'https://localhost:5001/api/deliveries/Update';

        const response = await fetch(address, {
            method: 'PATCH',
            body: JSON.stringify(req.body),
            headers: { 'Content-Type': 'application/json' },
            agent: httpAgent
        });

        let data = await response.json();
        res.status(200);
        return res.json(data);
    }
}