using EletricGo.Domain.Deliveries;
using EletricGo.Domain.Shared;
using EletricGo.Domain.Warehouses;
using EletricGo.Domain.Warehouses.DTO;
using EletricGo.Domain.Warehouses.ValueObjects;
using EletricGo.Services;
using Moq;

namespace WarehouseManagementTest.Unit.Services
{
    [TestFixture]
    internal class WarehouseServiceTest
    {
        private readonly string? id = "WH";

        private readonly string address = "Rua António Bernardino,47,4535-334,Porto";

        private readonly int altitude = 200;

        private readonly string latitude = "45";

        private readonly string longitude = "79";

        private readonly string description = "Porto";

        [Test]
        public void DefineDriverServiceConstructor()
        {
            var mockDeliveryRepo = new Mock<IWarehouseRepository>();
            var mockUnitWork = new Mock<IUnitOfWork>();

            var service = new WarehouseService(mockUnitWork.Object, mockDeliveryRepo.Object);

            Assert.That(service, Is.Not.Null);
        }


        [Test]
        public async Task GetWarehousesTest()
        {
            var mockRepository = new Mock<IWarehouseRepository>();
            mockRepository.Setup(repo => repo.GetAll()).ReturnsAsync(CreatedWarehouses());

            var mockUnit = new Mock<IUnitOfWork>();

            var service = new WarehouseService(mockUnit.Object, mockRepository.Object);

            var expectedList = await service.GetWarehouses();

            var resultList = CreatedWarehouses();

            Assert.That(resultList, Has.Count.EqualTo(expectedList.Count));
        }

        private List<Warehouse> CreatedWarehouses()
        {
            var warehouseList = new List<Warehouse>();
            warehouseList.Add(new Warehouse(new WarehouseId(id + "1"), new Address(address), new Altitude(altitude), new Coordinates(latitude, longitude), new Designation(description)));

            return warehouseList;
        }

        [Test]
        public async Task GetWarehouseTest()
        {
            var warehouseExpected = new Warehouse(new WarehouseId(id + "1"), new Address(address), new Altitude(altitude), new Coordinates(latitude, longitude), new Designation(description));

            var mockRepository = new Mock<IWarehouseRepository>();
            mockRepository.Setup(repo => repo.GetByID(new WarehouseId("WH1"))).ReturnsAsync(warehouseExpected);

            var mockUnit = new Mock<IUnitOfWork>();

            var service = new WarehouseService(mockUnit.Object, mockRepository.Object);

            var warehouseResult = await service.GetWarehouse(new WarehouseId(id + "1"));

            Assert.Multiple(() =>
            {
                Assert.That(warehouseResult.Id, Is.EqualTo(warehouseExpected.Id));
                Assert.That(warehouseResult.Address, Is.EqualTo(warehouseExpected.Address.fullAddress));
                Assert.That(warehouseResult.Designation, Is.EqualTo(warehouseExpected.Designation.designation));
                Assert.That(warehouseResult.Altitude, Is.EqualTo(warehouseExpected.Altitude.altitude));
                Assert.That(warehouseResult.Longitude, Is.EqualTo(warehouseExpected.Coordinates.longitude));
                Assert.That(warehouseResult.Latitude, Is.EqualTo(warehouseExpected.Coordinates.latitude));

            });
        }

        [Test]
        public async Task CreateWarehouseTest()
        {
            var warehouseExpected = new Warehouse(new WarehouseId(id + "1"), new Address(address), new Altitude(altitude), new Coordinates(latitude, longitude), new Designation(description));

            var warehouseDto = new WarehouseDto()
            {
                Id = "WH1",
                Address = address,
                Altitude = altitude,
                Latitude = latitude,
                Longitude = longitude,
                Designation = description
            };

            var mockRepository = new Mock<IWarehouseRepository>();
            var mockUnit = new Mock<IUnitOfWork>();
            mockRepository.Setup(repo => repo.Add(warehouseExpected));
            mockUnit.Setup(repo => repo.CommitAsync());

            mockRepository.Setup(repo => repo.GetByID(new WarehouseId(id + "1"))).ReturnsAsync(null as Warehouse);

            var service = new WarehouseService(mockUnit.Object, mockRepository.Object);

            var warehouseResult = await service.CreateWarehouse(warehouseDto);

            Assert.Multiple(() =>
            {
                Assert.That(warehouseResult.Id, Is.EqualTo(warehouseExpected.Id));
                Assert.That(warehouseResult.Address, Is.EqualTo(warehouseExpected.Address.fullAddress));
                Assert.That(warehouseResult.Altitude, Is.EqualTo(warehouseExpected.Altitude.altitude));
                Assert.That(warehouseResult.Latitude, Is.EqualTo(warehouseExpected.Coordinates.latitude));
                Assert.That(warehouseResult.Longitude, Is.EqualTo(warehouseExpected.Coordinates.longitude));
                Assert.That(warehouseResult.Designation, Is.EqualTo(warehouseExpected.Designation.designation));

            });
        }

        [Test]
        public async Task UpdateWarehouseTest()
        {
            var warehouseId = new WarehouseId(id + "1");

            var warehouseDto = new WarehouseDto()
            {
                Id = id + "1",
                Address = address,
                Altitude = 201,
                Latitude = latitude,
                Longitude = longitude,
                Designation = description
            };

            var warehouseExpected = new Warehouse(new WarehouseId(id + "1"), new Address(address), new Altitude(201), new Coordinates(latitude, longitude), new Designation(description));

            var mockRepository = new Mock<IWarehouseRepository>();
            mockRepository.Setup(repo => repo.GetByID(warehouseId)).ReturnsAsync(warehouseExpected);

            var mockUnit = new Mock<IUnitOfWork>();

            var service = new WarehouseService(mockUnit.Object, mockRepository.Object);

            var getDel = await service.UpdateWarehouse(warehouseDto);

            Assert.That(warehouseExpected.Altitude.altitude, Is.EqualTo(warehouseDto.Altitude));
        }


        [Test]
        public async Task DeleteAsyncTest()
        {
            var warehouseId = new WarehouseId(id + "1");

            var warehouseExpected = new Warehouse(new WarehouseId(id + "1"), new Address(address), new Altitude(201), new Coordinates(latitude, longitude), new Designation(description));

            var mockRepository = new Mock<IWarehouseRepository>();
            mockRepository.Setup(repo => repo.GetByID(warehouseId)).ReturnsAsync(warehouseExpected);
            mockRepository.Setup(repo => repo.Delete(warehouseExpected));

            var mockUnit = new Mock<IUnitOfWork>();

            var service = new WarehouseService(mockUnit.Object, mockRepository.Object);

            var warehouseResult = await service.DeleteWarehouse(warehouseId.warehouseID);

            Assert.Multiple(() =>
            {
                Assert.That(warehouseResult.Id, Is.EqualTo(warehouseExpected.Id));
                Assert.That(warehouseResult.Address, Is.EqualTo(warehouseExpected.Address.fullAddress));
                Assert.That(warehouseResult.Altitude, Is.EqualTo(warehouseExpected.Altitude.altitude));
                Assert.That(warehouseResult.Latitude, Is.EqualTo(warehouseExpected.Coordinates.latitude));
                Assert.That(warehouseResult.Longitude, Is.EqualTo(warehouseExpected.Coordinates.longitude));
                Assert.That(warehouseResult.Designation, Is.EqualTo(warehouseExpected.Designation.designation));

            });

        }

    }
}