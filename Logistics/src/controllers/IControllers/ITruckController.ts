import { Request, Response, NextFunction } from 'express';

export default interface ITruckController {
    getTruck(req: Request, res: Response, next: NextFunction);

    createTruck(req: Request, res: Response, next: NextFunction);

    updateTruck(req: Request, res: Response, next: NextFunction);

    deleteTruck(req: Request, res: Response, next: NextFunction);
}

