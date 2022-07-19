import { serverSideTranslations } from 'next-i18next/serverSideTranslations'

import { postController } from "backend/Post/controllerPost"
import { threadController } from "backend/Thread/controllerThread"
import { communityController } from "backend/User/controllerCommunity"
import { AuthGuard } from "frontend/pages/AuthGuard/AuthGuard"
import { Thread } from "frontend/pages/Thread/Thread"



export async function getServerSideProps({locale, params}) {

  const translations = await serverSideTranslations(locale, ["common"])

  const threadList = await threadController.listThreads([params.id]);
  const thread = threadList.map(x => JSON.parse(x))[0];

  let postList = await postController.listThreadPosts(thread._id)
  postList = postList.map(x => JSON.parse(x));

  const userIds = postList.map(p => p.userId);
  const uniqueUserIds = [ ... new Set(userIds)];
  let userList = await communityController.listUsers(uniqueUserIds);
  userList = userList.map(x => JSON.parse(x));
  
  return ({ props: {...translations, thread, postList, userList}})
}



export default function Handler(props) {
  
  return(
    <AuthGuard>
      <Thread
        thread={props.thread}
        postList={props.postList}
        userList={props.userList}
      />
    </AuthGuard>
  )
}