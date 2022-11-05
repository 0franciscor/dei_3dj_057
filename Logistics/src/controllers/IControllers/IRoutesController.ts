import { Request, Response, NextFunction } from 'express';

export default interface IRoutesController{

    getRoutes(req: Request, res: Response, next: NextFunction);
    
    getAllRoutes(req: Request, res: Response, next: NextFunction);

    createRoute(req: Request, res: Response, next: NextFunction);

    updateRoute(req: Request, res: Response, next: NextFunction);

    deleteRoute(req: Request, res: Response, next: NextFunction);

}