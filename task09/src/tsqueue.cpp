#include "tsqueue.h"

void threadsafe_queue_init(ThreadsafeQueue *q) {
    queue_init(&(q->q));
    pthread_mutex_init(&(q->m), nullptr);
}

void threadsafe_queue_destroy(ThreadsafeQueue *q) {
    queue_destroy(&(q->q));
    pthread_mutex_destroy(&(q->m));
}

void threadsafe_queue_push(ThreadsafeQueue *q, void *data) {
    pthread_mutex_lock(&(q->m));
    queue_push(&(q->q), data);
    pthread_cond_signal(&(q->cond));
    pthread_mutex_unlock(&(q->m));
}

void *threadsafe_queue_wait_and_pop(ThreadsafeQueue *q) {
    pthread_mutex_lock(&(q->m));
    while (queue_empty(&(q->q)))
        pthread_cond_wait(&(q->cond));
    void *data = queue_pop(&(q->q));
    pthread_mutex_unlock(&(q->m));
    return data;
}
