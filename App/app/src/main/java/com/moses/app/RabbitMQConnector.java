package com.moses.app;

import android.os.Build;
import android.os.Handler;

import androidx.annotation.RequiresApi;

import com.rabbitmq.client.AMQP;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.DeliverCallback;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.util.concurrent.TimeoutException;
import java.util.function.Function;

public class RabbitMQConnector {
    ConnectionFactory factory = new ConnectionFactory();

    private void setupConnection() throws NoSuchAlgorithmException, KeyManagementException, URISyntaxException, IOException, TimeoutException {
        factory.setUri("amqp://10.0.2.2:5672");

        Connection connection = factory.newConnection();

    }

    public void subscribe(MainActivity subscribent) {
        Thread subscribeThread = new Thread(new Runnable() {
            @RequiresApi(api = Build.VERSION_CODES.KITKAT)
            @Override
            public void run() {
                while(true) {
                    try {
                        Connection connection = factory.newConnection();
                        Channel channel = connection.createChannel();
                        channel.basicQos(1);
                        AMQP.Queue.DeclareOk q = channel.queueDeclare();
                        channel.queueBind(q.getQueue(), "moses_exchange", "mock_road");

                        DeliverCallback deliverCallback = (consumerTag, delivery) -> {
                            String message = new String(delivery.getBody(), StandardCharsets.UTF_8);
                            subscribent.showMessage(message);
                        };

                        channel.basicConsume(q.getQueue(), true, deliverCallback, consumerTag -> { });
                    } catch (TimeoutException e) {
                        e.printStackTrace();
                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
            }
        });
    }
}
