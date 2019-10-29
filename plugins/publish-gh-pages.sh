#!/bin/bash -x

set -e

staging=$1
deploy=$2
url=$3
branch=$4
remote=$5
cname=$6

if [[ -d $deploy && ! -d $deploy/.git ]]
then
   echo "Target directory $deploy exists and is not a git repository. Aborting" >&2
   exit 1
fi

if [[ ! -d $deploy ]]
then
    git clone --no-checkout --origin $remote $url $deploy
fi

cd $deploy

# safe and most reliable way to check if the branch exist
if git show-ref --verify --quiet refs/heads/$branch
then
    # if the branch exists locally
    git checkout $branch
elif git show-ref --verify --quiet refs/remotes/$remote/$branch
then
    # if the branch does not exist locally but exist in the specified remote ---
    # Note, git checkout $branch will search the branch with the same name with
    # ALL remotes, and set it as the tracking branch if there is a single such
    # remote, but does not allow the user to necessarily specify which.
    git checkout -b $branch --track $remote/$branch
else
    # if there is no matching branch, make an orphan branch
    git checkout --orphan $branch
fi

rsync -avz --delete --exclude .git/ --copy-links $staging $deploy

if [[ ! -z "$cname" ]]
then
    echo $cname > CNAME
fi

git add -A                      # add all changes in the worktree
git add $(git ls-files -o )     # add all untracked files in the worktree

git commit -m "Deployed on $(date)"

git push $remote $branch

