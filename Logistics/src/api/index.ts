import { Router } from 'express';
import truck from './routes/truckRoute';
import path from './routes/pathRoute';

export default () => {
	const app = Router();

	truck(app);
	path(app);
	
	return app
}
