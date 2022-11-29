import { Request, Response, NextFunction } from 'express';
import { Service } from 'typedi';

import IDeliveryController from "./IControllers/IDeliveryController";

import fetch from 'node-fetch';

const http = require('https');

@Service()
export default class DeliveryController implements IDeliveryController {
    constructor() { }

    public async getAllDeliveries(req: Request, res: Response, next: NextFunction) {

        const httpAgent = new http.Agent({ rejectUnauthorized: false });
        const address = 'https://localhost:5001/api/deliveries/GetAll';

        const response = await fetch(address, {
            method: 'GET',
            agent: httpAgent
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
        const httpAgent = new http.Agent({ rejectUnauthorized: false });
        const address = 'https://localhost:5001/api/deliveries/GetByID/' + req.params.id;

        const response = await fetch(address, {
            method: 'GET',
            agent: httpAgent
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
        const httpAgent = new http.Agent({ rejectUnauthorized: false });
        const address = 'https://localhost:5001/api/deliveries/CreateDelivery';

        const response = await fetch(address, {
            method: 'POST',
            body: JSON.stringify(req.body),
            headers: { 'Content-Type': 'application/json' },
            agent: httpAgent
        });

        if (response.status != 200) {
            res.status(response.status);
            return res.json({ message: "Error Creating Delivery"});
        }

        const address_prolog = 'https://vs-gate.dei.isep.ipp.pt:30382/create_delivery';
        const response_prolog = await fetch(address_prolog, {
            method: 'POST',
            body: JSON.stringify(req.body),
            headers: { 'Content-Type': 'application/json' },
            agent: httpAgent
        });
        const info = await response.json();
        res.status(200);
        return res.json(info);
    };


    public async updateDelivery(req: Request, res: Response, next: NextFunction) {
        const httpAgent = new http.Agent({ rejectUnauthorized: false });
        const address = 'https://localhost:5001/api/deliveries/Update';

        const response = await fetch(address, {
            method: 'PATCH',
            body: JSON.stringify(req.body),
            headers: { 'Content-Type': 'application/json' },
            agent: httpAgent
        });

        if (response.status != 200) {
            res.status(response.status);
            return res.json({ message: "Error Updating Delivery" });
        }

        const address_prolog = 'https://vs-gate.dei.isep.ipp.pt:30382/update_delivery';

        const response_prolog = await fetch(address_prolog, {
            method: 'PUT',
            body: JSON.stringify(req.body),
            headers: { 'Content-Type': 'application/json' },
            agent: httpAgent
        });
        
        const info = await response.json();
        res.status(200);
        return res.json(info);
    }
}