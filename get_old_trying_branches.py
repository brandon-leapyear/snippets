#!/usr/bin/env python3

import argparse
import json
import os
import subprocess
import tempfile
from pathlib import Path
from typing import NamedTuple, Optional

HERE = Path(os.path.abspath(__file__)).parent

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        '--repo',
        help='Repository to fetch',
        default='LeapYear/leapyear',
    )
    parser.add_argument(
        '--token',
        help='GitHub access token that can query the GitHub graphql API',
    )
    args = parser.parse_args()

    gql_executor = GqlExecutor(token=args.token)

    repo_owner, repo_name = args.repo.split('/')

    trying_branches = []
    has_next = True
    after = None
    while has_next:
        branches = gql_executor.query(BRANCHES_QUERY, {
            'repoOwner': repo_owner,
            'repoName': repo_name,
            'query': 'trying-',
            'after': after,
        })
        conn = branches['repository']['refs']

        for branch in conn['nodes']:
            name = branch['name']
            trying_branches.append(
                TryBranch(
                    id=branch['id'],
                    name=name,
                    pr_num=get_trying_pr_num(name),
                )
            )

        has_next = conn['pageInfo']['hasNextPage']
        if has_next:
            after = conn['pageInfo']['endCursor']

    print(f'Got trying branches:\n{[branch.name for branch in trying_branches]}')

    open_prs = set()
    has_next = True
    after = None
    while has_next:
        branches = gql_executor.query(OPEN_PRS_QUERY, {
            'repoOwner': repo_owner,
            'repoName': repo_name,
            'after': after,
        })
        conn = branches['repository']['pullRequests']

        open_prs.update(pr['number'] for pr in conn['nodes'])

        has_next = conn['pageInfo']['hasNextPage']
        if has_next:
            after = conn['pageInfo']['endCursor']

    print(f'\nGot open PRs:\n{open_prs}')

    stale_trying_branches = [
        branch
        for branch in trying_branches
        if branch.pr_num not in open_prs
    ]

    print(f'\nStale trying branches:\n{[branch.name for branch in stale_trying_branches]}')

    cmd = 'open -a "Google Chrome" {}'.format(
        ' '.join(
            f'https://github.com/{repo_owner}/{repo_name}/pull/{branch.pr_num}'
            for branch in stale_trying_branches
        )
    )
    print(f'\nCopy/paste the below in Terminal to open all PRs in Chrome:\n{cmd}')

    res = input('\nDelete branches? (y/n) ')
    if res != 'y':
        print('Aborted.')
        return

    for branch in stale_trying_branches:
        print(f'Deleting {branch.name}...')
        gql_executor.query(DELETE_BRANCH_MUTATION, {
            'branchId': branch.id,
        })

# Trying branches

class TryBranch(NamedTuple):
    id: str
    name: str
    pr_num: int

def get_trying_pr_num(trying_branch_name: str) -> int:
    res = trying_branch_name.split('-')
    if len(res) != 2:
        raise ValueError(f'Unknown trying branch name: {trying_branch_name}')
    return int(res[1])

# Graphql

class GqlExecutor(NamedTuple):
    token: Optional[str]

    def query(self, query: str, vars: dict):
        with tempfile.TemporaryDirectory() as tmpdir:
            tmpdir = Path(tmpdir)

            query_file = tmpdir / 'query.graphql'
            query_file.write_text(query)

            args = ['--vars', json.dumps(vars)]
            if self.token:
                args.extend(['--token', self.token])

            out = subprocess.check_output(
                [
                    HERE / 'gh_graphql.sh',
                    *args,
                    query_file,
                ],
                stderr=subprocess.DEVNULL,
            )

            response = json.loads(out.decode())

            if 'errors' in response:
                raise Exception(f'Got errors in response: {json.dumps(response, indent=4)}')

            return response['data']

BRANCHES_QUERY = '''
query (
    $repoOwner: String!,
    $repoName: String!,
    $query: String,
    $after: String
) {
    repository(owner: $repoOwner, name: $repoName) {
        refs(
            first: 20,
            refPrefix: "refs/heads/",
            query: $query,
            after: $after
        ) {
            pageInfo {
                hasNextPage
                endCursor
            }
            nodes {
                id
                name
            }
        }
    }
}
'''

OPEN_PRS_QUERY = '''
query (
    $repoOwner: String!,
    $repoName: String!,
    $after: String
) {
    repository(owner: $repoOwner, name: $repoName) {
        pullRequests(first: 20, after: $after, states: [OPEN]) {
            pageInfo {
                hasNextPage
                endCursor
            }
            nodes {
                number
            }
        }
    }
}
'''

DELETE_BRANCH_MUTATION = '''
mutation ($branchId: String!) {
    deleteRef(input: { refId: $branchId }) {
        clientMutationId
    }
}
'''

if __name__ == '__main__':
    main()
