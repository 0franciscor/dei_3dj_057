using EletricGo.Controllers;
using EletricGo.Domain.Deliveries;
using Moq;
using EletricGo.Domain.Shared;
using Microsoft.AspNetCore.Mvc;

namespace WarehouseManagementTest.Controllers.Deliveries
{
    [TestFixture]
    internal class DeliveriesIntegration
    {
        private readonly string? id = "testID";

        private readonly DateTime deliveryDate = new DateTime(2020, 12, 12);

        private readonly float loadTime = 10;

        private readonly float unloadTime = 20;

        private readonly string? destination = "testDestination";

        private readonly float deliveryMass = 30;
        

        //SETUP LISTS FOR GETALL()
        private List<Delivery> GetDeliveriesList()
        {
            var deliveryList = new List<Delivery>
            {
                new Delivery(id, new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass)),
                new Delivery("id2", new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass)),
                new Delivery("id3", new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass)),
                new Delivery("id4", new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass)),
                new Delivery("id5", new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass)),
            };

            return deliveryList;
        }

        [Test]
        public void DefineDriverServiceConstrutor()
        {
            var mockRepository = new Mock<IDeliveryRepository>();
            var mockUnit = new Mock<IUnitOfWork>();

            var service = new DeliveryService(mockUnit.Object, mockRepository.Object);

            Assert.That(service, Is.Not.Null);
        }

        [Test]
        public async Task GetAllTest()
        {
            var expectedList = GetDeliveriesList();

            var mockUnit = new Mock<IUnitOfWork>();
            var mockRepository = new Mock<IDeliveryRepository>();
            mockRepository.Setup(repo => repo.GetAll()).ReturnsAsync(expectedList);

            var deliveryService = new DeliveryService(mockUnit.Object, mockRepository.Object);
            var deliveryController = new DeliveryController(deliveryService);

            var resultList = await deliveryController.GetAll();

            for(int i = 0; i < expectedList.Count; i++)
            {
                Assert.AreEqual(expectedList[i].Id, resultList.Value[i].deliveryID);
            }
        }

        [Test]
        public async Task GetByIDTest()
        {
            var delivery = new Delivery(id, new DeliveryDate(deliveryDate), new LoadTime(loadTime), new UnloadTime(unloadTime), new Destination(destination), new DeliveryMass(deliveryMass));
            var deliveryExpected = delivery.toDeliveryDTO();
            var deliveryID = new DeliveryID(id);

            var mockUnit = new Mock<IUnitOfWork>();
            var mockRepository = new Mock<IDeliveryRepository>();
            mockRepository.Setup(repo => repo.GetByID(deliveryID)).ReturnsAsync(delivery);

            var deliveryService = new DeliveryService(mockUnit.Object, mockRepository.Object);

            var deliveryController = new DeliveryController(deliveryService);
            var aux = await deliveryController.GetByID(id);

            if (aux == null)
                Assert.Fail();
            else
            {
                var deliveryResult = ((DeliveryDTO)(aux.Result as OkObjectResult).Value);

                Assert.AreEqual(deliveryExpected.deliveryID, deliveryResult.deliveryID);
            }
        }


    }
}
