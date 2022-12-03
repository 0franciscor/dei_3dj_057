import { Request, Response, NextFunction } from 'express';

export default interface ITripController{

    getTrip(req: Request, res: Response, next: NextFunction);
    
    getAllTrips(req: Request, res: Response, next: NextFunction);

    createTrip(req: Request, res: Response, next: NextFunction);

    updateTrip(req: Request, res: Response, next: NextFunction);

    deleteTrip(req: Request, res: Response, next: NextFunction);

}