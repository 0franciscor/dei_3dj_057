import { Router } from 'express';
import pathRoute from './routes/pathRoute';
import roleRoute from './routes/roleRoute';
import user from './routes/userRoute';
import warehouseRoute from './routes/warehouseRoute';

export default () => {
	const app = Router();

	pathRoute(app);	
	user(app);
	warehouseRoute(app);
	roleRoute(app)
    return app;
}