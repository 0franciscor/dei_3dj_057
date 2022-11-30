import { Request,Response,NextFunction } from "express-serve-static-core";
import fetch from "node-fetch";
import { Service } from "typedi";
import IPackageController from "./IControllers/IPackageController";


@Service()
export default class packageController implements IPackageController {

constructor() {}


async createPackage(req: Request, res:Response, next: NextFunction) {
  const url = 'http://localhost:3000/api/packaging/';

  const data = req.body;
    const response = await fetch(url, {
      method: 'POST',
      body: JSON.stringify(data),
      headers: {
        'Content-Type': 'application/json'
      },
    })
    if(response.status != 201){
      res.status(response.status);
      return res.json({message: "Error creating truck"});
    }
    const info = await response.json();
    res.status(201);
    return res.json(info);

  };

    async getAllPackage(req: Request, res: Response, next: NextFunction) {
        const url = 'http://localhost:3000/api/packaging/all';
        const response = await fetch(url, {
          method: 'GET',
          headers: {
            'Accept': 'application/json'
          }
        });
        if(response.status != 200){
          res.status(response.status);
          return res.json({message: "Error getting all packages"});
        }
        const data = await response.json();
        res.status(200);
        return res.json(data);
      }



}