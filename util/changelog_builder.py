import os
import git

def get_commit_data(repo, start_hash, end_hash):
    commits = repo.iter_commits(rev=f"{start_hash}..{end_hash}")
    commit_data = []

    for commit in commits:
        commit_info = {
            "message": commit.message.split('\n', 1)[0],  # Get only first line of message
            "author": commit.author.name,
            "co_authors": [],
            "hash": commit.hexsha[:7]
        }

        commit_message = commit.message.strip()
        lines = commit_message.split('\n')

        for line in lines:
            if line.startswith("Co-authored-by:"):
                co_author = line.split(":")[1].strip().split(" ")[0]  # Take only first word (nickname)
                commit_info["co_authors"].append(co_author)

        commit_data.append(commit_info)

    return commit_data

def get_last_tag_hash(repo):
    tags = repo.tags

    # Sort tags by commit timestamp in reverse order
    sorted_tags = sorted(tags, key=lambda x: x.commit.committed_datetime, reverse=True)

    last_tag = sorted_tags[0]
    last_tag_commit = last_tag.commit
    last_tag_hash = last_tag_commit.hexsha

    print("Name of the last tag:", last_tag.name)
    print("Hash of the last tag:", last_tag_hash)

    return last_tag_hash

def format_commit(commit):
    formatted_co_authors = ", ".join([f"@{author}" for author in commit["co_authors"]])
    if formatted_co_authors:
        return f"- {commit['message']} (@{commit['author']}, {formatted_co_authors})"
    else:
        return f"- {commit['message']} (@{commit['author']})"

def main():
    git_path = os.getcwd()
    print("Current directory:", git_path)

    repo = git.Repo(git_path)
    print("Current branch:", repo.active_branch)

    start_hash = get_last_tag_hash(repo)
    end_hash = 'HEAD'

    commit_data = get_commit_data(repo, start_hash, end_hash)
    output_file_path = 'changelog.txt'
    with open(output_file_path, 'w', encoding='utf-8') as output_file:
        for commit in commit_data:
            print(format_commit(commit))
            output_file.write(format_commit(commit) + '\n')

    print(f"Changelog has been written to {output_file_path}")

if __name__ == "__main__":
    main()
