import { Router } from 'express';
import truck from './routes/truckRoute';
import path from './routes/pathRoute';
import packaging from './routes/packagingRoute';
import route from './routes/routeRoute';

export default () => {
	const app = Router();

	truck(app);
	
	path(app);
	packaging(app);
	route(app);
	
	return app
}
