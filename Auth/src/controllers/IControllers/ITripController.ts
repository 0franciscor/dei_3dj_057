import { Request, Response, NextFunction } from 'express';

export default interface ITripController  {
    createTrip(req: Request, res: Response, next: NextFunction);
    
}